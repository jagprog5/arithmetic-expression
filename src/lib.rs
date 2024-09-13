#![no_std]

#[macro_export]
macro_rules! arith_expr_impl {
    // CodeUnit should be like u32 or u8
    ($CodeUnit: ty) => {
        type CodeUnit = $CodeUnit;

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum TokenRawData<'a> {
            Invalid,
            Add,
            Sub,
            Mul,
            LeftBracket,
            RightBracket,
            Xor,
            And,
            Or,
            Equal,
            Complement,
            NotEqual,
            LessThan,
            LessThanEqual,
            LeftShift,
            GreaterThan,
            GreaterThanEqual,
            RightShift,
            Number(CodeUnit),
            Symbol(&'a [u8]),
            UnaryAdd,
            UnarySub,
        }

        impl<'a> Default for TokenRawData<'a> {
            fn default() -> Self {
                TokenRawData::Invalid
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct TokenRaw<'a> {
            data: TokenRawData<'a>,
            offset: usize,
        }

        impl<'a> Default for TokenRaw<'a> {
            fn default() -> Self {
                Self {
                    data: Default::default(),
                    offset: Default::default(),
                }
            }
        }

        /// this function should be called in two passes. for the first pass, give None
        /// as the output arg, and the return value is the size of the output for the
        /// second pass. on err, gives the offending location and error reason
        pub fn tokenize<'a>(
            input: &'a [u8],
            output: &mut Option<&mut [TokenRaw<'a>]>,
        ) -> Result<usize, (usize, &'static str)> {
            struct ParseState {
                following_op_is_unary: bool,
                value_allowed_next: bool,
            }

            fn send_output<'a>(
                token_to_send: TokenRaw<'a>,
                ret: &mut usize,
                output: &mut Option<&mut [TokenRaw<'a>]>,
                parse_state: &mut ParseState,
            ) {
                // update the parse state based on the sent token
                // (the previous token affects what the next token is allowed to be)

                parse_state.value_allowed_next = match token_to_send.data {
                    TokenRawData::RightBracket //.
                    | TokenRawData::Number(_) //.
                    | TokenRawData::Symbol(_) => false, // value after value not allowed
                    _ => true,
                };

                parse_state.following_op_is_unary = match token_to_send.data {
                    TokenRawData::RightBracket
                    | TokenRawData::Number(_)
                    | TokenRawData::Symbol(_)
                    | TokenRawData::UnaryAdd
                    | TokenRawData::UnarySub
                    | TokenRawData::Complement => false,
                    _ => true,
                };

                if let Some(o) = output {
                    o[*ret] = token_to_send;
                }

                *ret += 1;
            }

            fn send_zero_before_unary<'a>(
                token_to_send: TokenRaw<'a>,
                ret: &mut usize,
                output: &mut Option<&mut [TokenRaw<'a>]>,
                parse_state: &mut ParseState,
            ) {
                debug_assert!(match token_to_send.data {
                    TokenRawData::UnaryAdd //.
                    | TokenRawData::UnarySub //.
                    | TokenRawData::Complement => true,
                    _ => false,
                });

                let zero_to_send = TokenRaw {
                    offset: token_to_send.offset,
                    data: TokenRawData::Number(0),
                };

                send_output(zero_to_send, ret, output, parse_state);
                send_output(token_to_send, ret, output, parse_state);
            }

            let mut ret: usize = 0;
            let mut parse_state = ParseState {
                following_op_is_unary: true,
                value_allowed_next: true,
            };

            let mut input_iter = input.iter().enumerate().peekable();
            'outer: loop {
                let (i, ch) = match input_iter.next() {
                    None => break,
                    Some(v) => v,
                };
                match ch {
                    b' ' | //.
                    b'\t'| //.
                    b'\r'| //.
                    b'\n' => continue,
                    b'+' | // .
                    b'-' => {
                        let token_to_send = TokenRaw {
                            offset: i,
                            data: if *ch == b'+' {
                                if parse_state.following_op_is_unary {
                                    TokenRawData::UnaryAdd
                                } else {
                                    TokenRawData::Add
                                }
                            } else {
                                if parse_state.following_op_is_unary {
                                    TokenRawData::UnarySub
                                } else {
                                    TokenRawData::Sub
                                }
                            },
                        };
                        match token_to_send.data {
                            TokenRawData::UnaryAdd | TokenRawData::UnarySub => {
                                send_zero_before_unary(token_to_send, &mut ret, output, &mut parse_state)
                            },
                            _ => {
                                send_output(token_to_send, &mut ret, output, &mut parse_state)
                            }
                        }
                    }
                    b'~' => {
                        if !parse_state.following_op_is_unary {
                            return Err((i, "previous token forbids unary op"))
                        }
                        let token_to_send = TokenRaw {
                            offset: i,
                            data: TokenRawData::Complement,
                        };
                        send_zero_before_unary(token_to_send, &mut ret, output, &mut parse_state)
                    },
                    b'(' => {
                        if !parse_state.value_allowed_next {
                            return Err((i, "previous token forbids value"))
                        }
                        let token_to_send = TokenRaw {
                            offset: i,
                            data: TokenRawData::LeftBracket,
                        };
                        send_output(token_to_send, &mut ret, output, &mut parse_state)
                    }
                    b'*' | // .
                    b')' | // .
                    b'^' | // .
                    b'&' | // .
                    b'|' | // .
                    b'=' => {
                        let token_to_send = TokenRaw {
                            offset: i,
                            data: match *ch {
                                b'*' => TokenRawData::Mul,
                                b')' => TokenRawData::RightBracket,
                                b'^' => TokenRawData::Xor,
                                b'&' => TokenRawData::And,
                                b'|' => TokenRawData::Or,
                                b'=' | _ => TokenRawData::Equal,
                            },
                        };
                        send_output(token_to_send, &mut ret, output, &mut parse_state);
                    }
                    b'<' | //.
                    b'>' => {
                        if let Some((_, peek_ch)) = input_iter.peek() {
                            if **peek_ch == b'=' {
                                let token_to_send = TokenRaw {
                                    offset: i,
                                    data: if *ch == b'<' { TokenRawData::LessThanEqual } else { TokenRawData::GreaterThanEqual },
                                };
                                send_output(token_to_send, &mut ret, output, &mut parse_state);
                                input_iter.next();
                            } else if **peek_ch == *ch {
                                let token_to_send = TokenRaw {
                                    offset: i,
                                    data: if *ch == b'<' { TokenRawData::LeftShift } else { TokenRawData::RightShift },
                                };
                                send_output(token_to_send, &mut ret, output, &mut parse_state);
                                input_iter.next();
                            } else {
                                // the peeked character was not used
                                let token_to_send = TokenRaw {
                                    offset: i,
                                    data: if *ch == b'<' { TokenRawData::LessThan} else { TokenRawData::GreaterThan },
                                };
                                send_output(token_to_send, &mut ret, output, &mut parse_state);
                            }
                        } else {
                            let token_to_send = TokenRaw {
                                offset: i,
                                data: if *ch == b'<' { TokenRawData::LessThan} else { TokenRawData::GreaterThan },
                            };
                            send_output(token_to_send, &mut ret, output, &mut parse_state);
                        }
                    }
                    b'!' => {
                        let op_offset_begin = i;

                        let (_, ch) = match input_iter.next() {
                            None => return Err((op_offset_begin, "operator incomplete (!=)")),
                            Some(v) => v,
                        };
                        if *ch != b'=' {
                            return Err((op_offset_begin, "operator incomplete (!=)"))
                        } else {
                            let token_to_send = TokenRaw {
                                offset: op_offset_begin,
                                data: TokenRawData::NotEqual,
                            };
                            send_output(token_to_send, &mut ret, output, &mut parse_state);
                        }
                    }
                    b'\'' => {
                        let literal_offset_begin = i;
                        if !parse_state.value_allowed_next {
                            return Err((i, "previous token forbids value"))
                        }
                        match input_iter.next() {
                            None => return Err((literal_offset_begin, "expecting literal content")),
                            Some((_, ch)) => {
                                // handle the first character in literal
                                if *ch != b'\\' {
                                    let token_to_send = TokenRaw {
                                        offset: literal_offset_begin,
                                        data: TokenRawData::Number(*ch as CodeUnit),
                                    };
                                    send_output(token_to_send, &mut ret, output, &mut parse_state);
                                } else {
                                    // beginning of escape sequence
                                    let (i, ch) = match input_iter.next() {
                                        None => return Err((literal_offset_begin, "expecting escaped literal content")),
                                        Some(v) => v,
                                    };
                                    // handle first character in escape sequence
                                    let mut literal_value: CodeUnit = match *ch {
                                        b'0' | b'x' => 0,
                                        b'a' => 7,
                                        b'b' => 8,
                                        b't' => '\t' as CodeUnit,
                                        b'n' => '\n' as CodeUnit,
                                        b'v' => 11,
                                        b'f' => 12,
                                        b'r' => 13,
                                        b'e' => 27,
                                        b'\\' => '\\' as CodeUnit,
                                        _ => return Err((i, "invalid escaped character")),
                                    };

                                    if *ch != b'x' {
                                        let token_to_send = TokenRaw {
                                            offset: literal_offset_begin,
                                            data: TokenRawData::Number(literal_value),
                                        };
                                        send_output(token_to_send, &mut ret, output, &mut parse_state);
                                    } else {
                                        for _ in 0..(core::mem::size_of::<CodeUnit>() * 2) { // 2 nibbles per byte
                                            let (_, ch) = match input_iter.next() {
                                                None => return Err((literal_offset_begin, "expecting escaped hex literal content")),
                                                Some(v) => v,
                                            };
                                            if *ch >= b'0' && *ch <= b'9' {
                                                literal_value <<= 4;
                                                literal_value += *ch as CodeUnit - '0' as CodeUnit;
                                            } else if *ch >= b'a' && *ch <= b'f' {
                                                literal_value <<= 4;
                                                literal_value += (*ch as CodeUnit - b'a' as CodeUnit) + 10;
                                            } else if *ch >= b'A' && *ch <= b'F' {
                                                literal_value <<= 4;
                                                literal_value += (*ch as CodeUnit - b'A' as CodeUnit) + 10;
                                            } else if *ch == b'\'' {
                                                let token_to_send = TokenRaw {
                                                    offset: literal_offset_begin,
                                                    data: TokenRawData::Number(literal_value),
                                                };
                                                send_output(token_to_send, &mut ret, output, &mut parse_state);
                                                continue 'outer;
                                            } else {
                                                return Err((literal_offset_begin, "invalid hex escaped character"));
                                            }
                                        }
                                        let token_to_send = TokenRaw {
                                            offset: literal_offset_begin,
                                            data: TokenRawData::Number(literal_value),
                                        };
                                        send_output(token_to_send, &mut ret, output, &mut parse_state);
                                    }
                                }
                            }
                        }
                        match input_iter.next() {
                            None => return Err((literal_offset_begin, "expecting end of literal")),
                            Some((_, ch)) => {
                                if *ch != b'\'' {
                                    return Err((literal_offset_begin, "expecting end of literal"));
                                }
                            }
                        };
                    }
                    b'0' | // .
                    b'1' | // .
                    b'2' | // .
                    b'3' | // .
                    b'4' | // .
                    b'5' | // .
                    b'6' | // .
                    b'7' | // .
                    b'8' | // .
                    b'9' => {
                        let number_offset_begin = i;
                        if !parse_state.value_allowed_next {
                            return Err((number_offset_begin, "previous token forbids value"))
                        }
                        let mut number_value : CodeUnit = *ch as CodeUnit - '0' as CodeUnit;
                        loop {
                            if let Some((_, peek_ch)) = input_iter.peek() {
                                if **peek_ch < b'0' || **peek_ch > b'9' {
                                    let token_to_send = TokenRaw {
                                        offset: number_offset_begin,
                                        data: TokenRawData::Number(number_value),
                                    };
                                    send_output(token_to_send, &mut ret, output, &mut parse_state);
                                    break;
                                } else {
                                    match number_value.checked_mul(10) {
                                        None => { return Err((number_offset_begin, "number too large")); }
                                        Some(v) => { number_value = v; },
                                    }
                                    match number_value.checked_add(**peek_ch as CodeUnit - b'0' as CodeUnit) {
                                        None => { return Err((number_offset_begin, "number too large")); }
                                        Some(v) => { number_value = v; },
                                    }
                                    // the peeked ch is consumed
                                    input_iter.next();
                                }
                            } else {
                                let token_to_send = TokenRaw {
                                    offset: number_offset_begin,
                                    data: TokenRawData::Number(number_value),
                                };
                                send_output(token_to_send, &mut ret, output, &mut parse_state);
                                break;
                            }
                        }
                    }
                    _ => {
                        if (*ch >= b'a' && *ch <= b'z') || (*ch >= b'A' && *ch <= b'Z' || *ch == b'_') {
                            if !parse_state.value_allowed_next {
                                return Err((i, "previous token forbids value"))
                            }
                            // beginning of slice is at current position
                            let symbol_index_begin = i;
                            let mut symbol_len: usize = 0;
                            loop {
                                symbol_len += 1;
                                if let Some((_, peek_ch)) = input_iter.peek() {
                                    if !((**peek_ch >= b'a' && **peek_ch <= b'z') || (**peek_ch >= b'A' && **peek_ch <= b'Z') || (**peek_ch >= b'0' && **peek_ch <= b'9') || **peek_ch == b'_') {
                                        let slice = &input[symbol_index_begin..symbol_index_begin + symbol_len];
                                        let token_to_send = TokenRaw {
                                            offset: symbol_index_begin,
                                            data: TokenRawData::Symbol(slice),
                                        };
                                        send_output(token_to_send, &mut ret, output, &mut parse_state);
                                        break;
                                    }
                                    input_iter.next();
                                } else {
                                    // end of string
                                    let slice = &input[symbol_index_begin..symbol_index_begin + symbol_len];
                                    let token_to_send = TokenRaw {
                                        offset: symbol_index_begin,
                                        data: TokenRawData::Symbol(slice),
                                    };
                                    send_output(token_to_send, &mut ret, output, &mut parse_state);
                                    break;
                                }
                            }
                        } else {
                            return Err((i, "invalid unit"))
                        }
                    }
                }
            }

            Ok(ret)
        }

        type SymbolLookupIndex = usize;

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Parsed {
            Invalid,
            Add,
            Sub,
            Mul,
            LeftBracket,
            RightBracket,
            Xor,
            And,
            Or,
            Equal,
            Complement,
            NotEqual,
            LessThan,
            LessThanEqual,
            LeftShift,
            GreaterThan,
            GreaterThanEqual,
            RightShift,
            Number(CodeUnit),
            Symbol(SymbolLookupIndex),
            UnaryAdd,
            UnarySub,
        }

        impl Default for Parsed {
            fn default() -> Self {
                Parsed::Invalid
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct Token {
            data: Parsed,
            offset: usize,
        }

        impl Default for Token {
            fn default() -> Self {
                Self {
                    data: Default::default(),
                    offset: Default::default(),
                }
            }
        }

        pub fn resolve_symbols(
            raw_tokens_input: &[TokenRaw<'_>],
            allowed_symbols: &[&[u8]],
            tokens_output: &mut [Token],
        ) -> Result<(), (usize, &'static str)> {
            debug_assert!(raw_tokens_input.len() == tokens_output.len());
            let mut output_iter = tokens_output.iter_mut();
            for in_elem in raw_tokens_input.iter() {
                let out_elem = match output_iter.next() {
                    None => return Err((0, "resolve symbol allocation error")),
                    Some(v) => v,
                };
                out_elem.offset = in_elem.offset;
                out_elem.data = match in_elem.data {
                    TokenRawData::Invalid => Parsed::Invalid,
                    TokenRawData::Add => Parsed::Add,
                    TokenRawData::Sub => Parsed::Sub,
                    TokenRawData::Mul => Parsed::Mul,
                    TokenRawData::LeftBracket => Parsed::LeftBracket,
                    TokenRawData::RightBracket => Parsed::RightBracket,
                    TokenRawData::Xor => Parsed::Xor,
                    TokenRawData::And => Parsed::And,
                    TokenRawData::Or => Parsed::Or,
                    TokenRawData::Equal => Parsed::Equal,
                    TokenRawData::Complement => Parsed::Complement,
                    TokenRawData::NotEqual => Parsed::NotEqual,
                    TokenRawData::LessThan => Parsed::LessThan,
                    TokenRawData::LessThanEqual => Parsed::LessThanEqual,
                    TokenRawData::LeftShift => Parsed::LeftShift,
                    TokenRawData::GreaterThan => Parsed::GreaterThan,
                    TokenRawData::GreaterThanEqual => Parsed::GreaterThanEqual,
                    TokenRawData::RightShift => Parsed::RightShift,
                    TokenRawData::Number(v) => Parsed::Number(v),
                    TokenRawData::UnaryAdd => Parsed::UnaryAdd,
                    TokenRawData::UnarySub => Parsed::UnarySub,
                    TokenRawData::Symbol(s) => {
                        let mut ret: Option<SymbolLookupIndex> = None;
                        for (i, a) in allowed_symbols.iter().enumerate() {
                            if *a == s {
                                ret = Some(i);
                                break;
                            }
                        }
                        if let Some(i) = ret {
                            Parsed::Symbol(i)
                        } else {
                            return Err((in_elem.offset, "invalid symbol"));
                        }
                    }
                };
            }
            Ok(())
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct ParseInfo {
            /// the stack len needed to interpret this expression
            pub interpret_stack_size: usize,
            /// the len of the output
            pub output_size: usize,
        }

        /// the output is populated in reverse polish notation.  
        /// the input will be scrambled into a undefined order (used as scratch space as its consumed); should be discarded after use by this function.  
        /// on err, returns the offending location and reason.  
        pub fn parse(
            input_tokens: &mut [Token],
            output_parsed: &mut [Parsed],
        ) -> Result<ParseInfo, (usize, &'static str)> {
            // https://math.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/
            debug_assert!(input_tokens.len() == output_parsed.len());

            // consuming from the input is guaranteed to be the same speed or
            // faster than producing to it as stack space
            let mut operator_stack_len: usize = 0; // index within input_tokens

            let mut interpret_stack_len: usize = 0;
            let mut interpret_stack_len_max: usize = 0;

            let mut output_index: usize = 0;

            fn send_output(token_to_send: Parsed, output_index: &mut usize, output_parsed: &mut [Parsed]) {
                output_parsed[*output_index] = token_to_send;
                *output_index += 1;
            }

            fn push_operator_stack(
                token_to_push: Token,
                operator_stack_len: &mut usize,
                input_tokens: &mut [Token],
            ) {
                input_tokens[*operator_stack_len] = token_to_push;
                *operator_stack_len += 1;
            }

            fn pop_operator_stack(
                operator_stack_len: &mut usize,
                input_tokens: &mut [Token],
            ) -> Option<Token> {
                if *operator_stack_len == 0 {
                    return None;
                }
                *operator_stack_len -= 1;
                Some(input_tokens[*operator_stack_len])
            }

            fn send_op_to_output(
                interpret_stack_len: &mut usize,
                op_stack_top: Token,
                output_index: &mut usize,
                output_parsed: &mut [Parsed],
            ) -> Result<(), (usize, &'static str)> {
                if *interpret_stack_len < 2 {
                    return Err((op_stack_top.offset, "stack exhausted"));
                }
                *interpret_stack_len -= 1;
                let token_to_send = match op_stack_top.data {
                    Parsed::UnaryAdd => Parsed::Add,
                    Parsed::UnarySub => Parsed::Sub,
                    _ => op_stack_top.data,
                };
                output_parsed[*output_index] = token_to_send;
                *output_index += 1;
                Ok(())
            }

            for i in 0..input_tokens.len() {
                let in_elem = input_tokens[i];

                match in_elem.data {
                    Parsed::Number(_) | Parsed::Symbol(_) => {
                        // "If the incoming symbol is an operand, print it."
                        interpret_stack_len += 1;
                        if interpret_stack_len > interpret_stack_len_max {
                            interpret_stack_len_max = interpret_stack_len;
                        }
                        send_output(in_elem.data, &mut output_index, output_parsed);
                    }
                    Parsed::LeftBracket => {
                        // "If the incoming symbol is a left parenthesis, push it on the stack"
                        push_operator_stack(in_elem, &mut operator_stack_len, input_tokens);
                    }
                    Parsed::RightBracket => {
                        // "If the incoming symbol is a right parenthesis: discard the right
                        // parenthesis, pop and print the stack symbols until you see a left
                        // parenthesis. Pop the left parenthesis and discard it."
                        loop {
                            let op_stack_top =
                                match pop_operator_stack(&mut operator_stack_len, input_tokens) {
                                    None => return Err((in_elem.offset, "no matching left bracket")),
                                    Some(v) => v,
                                };
                            if let Parsed::LeftBracket = op_stack_top.data {
                                break;
                            }
                            if let Err(e) = send_op_to_output(
                                &mut interpret_stack_len,
                                op_stack_top,
                                &mut output_index,
                                output_parsed,
                            ) {
                                return Err(e);
                            }
                        }
                    }
                    _ => {
                        fn token_precedence(p: Parsed) -> usize {
                            match p {
                                Parsed::UnaryAdd | // .
                                Parsed::UnarySub | // .
                                Parsed::Complement => 0,
                                Parsed::Mul => 1,
                                Parsed::Add | // .,
                                Parsed::Sub => 2,
                                Parsed::LeftShift | // .,
                                Parsed::RightShift => 3,
                                Parsed::LessThan | // .
                                Parsed::LessThanEqual => 4,
                                Parsed::GreaterThan | // .,
                                Parsed::GreaterThanEqual => 5,
                                Parsed::And => 6,
                                Parsed::Xor => 7,
                                Parsed::Or => 8,
                                Parsed::Equal | // .
                                Parsed::NotEqual => 9,
                                _ => 10, // left bracket is lowest precedence - for logic below
                            }
                        }
                        // the incoming symbol is an operator
                        let input_token_precedence = token_precedence(in_elem.data);
                        loop {
                            // "[If] the stack is empty or contains a left parenthesis on top,
                            // push the incoming operator onto the stack"
                            let op_stack_top =
                                match pop_operator_stack(&mut operator_stack_len, input_tokens) {
                                    None => {
                                        push_operator_stack(in_elem, &mut operator_stack_len, input_tokens);
                                        break;
                                    }
                                    Some(v) => v,
                                };
                            let stack_top_token_precedence = token_precedence(op_stack_top.data);
                            if input_token_precedence < stack_top_token_precedence {
                                // "If the incoming symbol is an operator and has either
                                // higher precedence than the operator on the top of the
                                // stack, ... or if the stack is empty, or if the top of
                                // the stack is "(" (a floor) -- push it on the stack."
                                push_operator_stack(op_stack_top, &mut operator_stack_len, input_tokens);
                                push_operator_stack(in_elem, &mut operator_stack_len, input_tokens);
                                break;
                            } else {
                                // "If the incoming symbol is an operator and has either
                                // lower precedence than the operator on the top of the
                                // stack, or has the same precedence as the operator on
                                // the top of the stack ... -- continue to pop the stack
                                // until this is not true. Then, push the incoming
                                // operator."
                                if let Err(e) = send_op_to_output(
                                    &mut interpret_stack_len,
                                    op_stack_top,
                                    &mut output_index,
                                    output_parsed,
                                ) {
                                    return Err(e);
                                }
                            }
                        }
                    }
                }
            }

            // "At the end of the expression, pop and print all operators on the stack.
            // (No parentheses should remain.)"
            loop {
                let op_stack_top = match pop_operator_stack(&mut operator_stack_len, input_tokens) {
                    None => break,
                    Some(v) => v,
                };
                if let Parsed::LeftBracket = op_stack_top.data {
                    return Err((op_stack_top.offset, "no matching right bracket"));
                }
                if let Err(e) = send_op_to_output(
                    &mut interpret_stack_len,
                    op_stack_top,
                    &mut output_index,
                    output_parsed,
                ) {
                    return Err(e);
                }
            }

            debug_assert_eq!(interpret_stack_len, 1);

            if output_index == 0 {
                return Err((0, "empty expression not allowed"));
            }

            Ok(ParseInfo {
                interpret_stack_size: interpret_stack_len_max,
                output_size: output_index,
            })
        }

        /// expr points to the initialized range populated by parse (length indicated by ParseInfo)  
        /// interpret_stack is a scratch space for this function (length indicated by ParseInfo)  
        /// symbol_value indicates the value taken on by each symbol passed to resolve_symbols  
        pub fn interpret(expr: &[Parsed], interpret_stack: &mut [CodeUnit], symbol_value: &[CodeUnit]) -> CodeUnit {
            let mut interpret_stack_len: usize = 0;
            for in_elem in expr.iter() {
                match in_elem {
                    Parsed::Number(v) => {
                        interpret_stack[interpret_stack_len] = *v;
                        interpret_stack_len += 1;
                    }
                    Parsed::Symbol(v) => {
                        interpret_stack[interpret_stack_len] = symbol_value[*v];
                        interpret_stack_len += 1;
                    }
                    _ => {
                        let rhs = interpret_stack[interpret_stack_len - 1];
                        let result = &mut interpret_stack[interpret_stack_len - 2];
                        let lhs = *result;
                        interpret_stack_len -= 1;
                        match in_elem {
                            Parsed::Add => {
                                *result = lhs.wrapping_add(rhs);
                            }
                            Parsed::Sub => {
                                *result = lhs.wrapping_sub(rhs);
                            }
                            Parsed::Mul => {
                                *result = lhs.wrapping_mul(rhs);
                            }
                            Parsed::Xor => {
                                *result = lhs ^ rhs;
                            }
                            Parsed::Complement => {
                                *result = !rhs;
                            }
                            Parsed::Equal => {
                                *result = (lhs == rhs) as CodeUnit;
                            }
                            Parsed::NotEqual => {
                                *result = (lhs != rhs) as CodeUnit;
                            }
                            Parsed::LessThan => {
                                *result = (lhs < rhs) as CodeUnit;
                            }
                            Parsed::LessThanEqual => {
                                *result = (lhs <= rhs) as CodeUnit;
                            }
                            Parsed::GreaterThan => {
                                *result = (lhs > rhs) as CodeUnit;
                            }
                            Parsed::GreaterThanEqual => {
                                *result = (lhs >= rhs) as CodeUnit;
                            }
                            Parsed::LeftShift => {
                                *result = lhs.wrapping_shl(rhs.into());
                            }
                            Parsed::RightShift => {
                                *result = lhs.wrapping_shr(rhs.into());
                            }
                            Parsed::And => {
                                *result = lhs & rhs;
                            }
                            _ => {
                                debug_assert!(match in_elem {
                                    Parsed::Or => true,
                                    _ => false,
                                });
                                *result = lhs | rhs;
                            }
                        }
                    }
                }
            }
            interpret_stack[0]
        }
    };
}

arith_expr_impl!(u32);

#[cfg(test)]
mod test_tokenization {
    use super::*;

    #[test]
    fn basic() {
        let input = [b'+', b' ', b'-'];

        let cap = tokenize(&input, &mut None);
        assert_eq!(cap, Ok(3));
        let mut output = [TokenRaw::default(); 3];
        let cap = tokenize(&input, &mut Some(&mut output));
        assert_eq!(cap, Ok(3));
        let mut expected_output = [TokenRaw::default(); 3];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(0),
        };
        expected_output[1] = TokenRaw {
            offset: 0,
            data: TokenRawData::UnaryAdd,
        };
        expected_output[2] = TokenRaw {
            offset: 2,
            data: TokenRawData::Sub,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn num_end_with_string() {
        let input = [b'1', b'2', b'3'];

        let cap = tokenize(&input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(&input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(123),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn num_end_any_other_reason() {
        let input = [b'1', b'2', b'3', b'\n', b'\r', b'\t'];

        let cap = tokenize(&input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(&input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(123),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn number_peek() {
        let input = b"123=";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 2;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(123),
        };
        expected_output[1] = TokenRaw {
            offset: 3,
            data: TokenRawData::Equal,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn num_max() {
        if CodeUnit::MIN == 0 && core::mem::size_of::<CodeUnit>() == 4 {
            // skip this test if not u32
            let input = b"4294967295";

            let cap = tokenize(input, &mut None);
            const OUTPUT_SIZE: usize = 1;
            assert_eq!(cap, Ok(OUTPUT_SIZE));
            let mut output = [TokenRaw::default(); OUTPUT_SIZE];
            let cap = tokenize(input, &mut Some(&mut output));
            assert_eq!(cap, Ok(OUTPUT_SIZE));
            let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
            expected_output[0] = TokenRaw {
                offset: 0,
                data: TokenRawData::Number(CodeUnit::MAX),
            };
            let expected_output = expected_output;
            assert_eq!(output, expected_output);
        }
    }

    #[test]
    fn num_overflow_add() {
        let input = b"4294967296";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn num_overflow_mul() {
        let input = b"42949672950";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn symbol_whole_string() {
        let input = b"abz9_hello";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Symbol(input),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn symbol_peek() {
        let input = b"abz9_hello=";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 2;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Symbol(&input[..10]),
        };
        expected_output[1] = TokenRaw {
            offset: 10,
            data: TokenRawData::Equal,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn symbol_end_other_reason() {
        let input = b"abz9_hello\n\t\r";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Symbol(&input[..10]),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn invalid_unit() {
        let input = b"@ 123";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn less_than() {
        let input = b"<";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::LessThan,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn less_than_peek() {
        let input = b"<1";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 2;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::LessThan,
        };
        expected_output[1] = TokenRaw {
            offset: 1,
            data: TokenRawData::Number(1),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn less_than_equal() {
        let input = b"<=";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::LessThanEqual,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn less_than_equal_end_of_string() {
        let input = b"<=\n\t\r";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::LessThanEqual,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn greater_than() {
        let input = b">";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::GreaterThan,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn greater_than_equal() {
        let input = b">=";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::GreaterThanEqual,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn shifts() {
        let input = b"<<>><>";

        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 4;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::LeftShift,
        };
        expected_output[1] = TokenRaw {
            offset: 2,
            data: TokenRawData::RightShift,
        };
        expected_output[2] = TokenRaw {
            offset: 4,
            data: TokenRawData::LessThan,
        };
        expected_output[3] = TokenRaw {
            offset: 5,
            data: TokenRawData::GreaterThan,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn not_equal_abrupt_end() {
        let input = b"!";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn not_equal_unexpected() {
        let input = b"!5";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn not_equal() {
        let input = b"!==";
        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 2;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::NotEqual,
        };
        expected_output[1] = TokenRaw {
            offset: 2,
            data: TokenRawData::Equal,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn unary_op_not_after_value() {
        let input = b"15~3";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn brackets() {
        let input = b"(~3)";
        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 5;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::LeftBracket,
        };
        expected_output[1] = TokenRaw {
            offset: 1,
            data: TokenRawData::Number(0),
        };
        expected_output[2] = TokenRaw {
            offset: 1,
            data: TokenRawData::Complement,
        };
        expected_output[3] = TokenRaw {
            offset: 2,
            data: TokenRawData::Number(3),
        };
        expected_output[4] = TokenRaw {
            offset: 3,
            data: TokenRawData::RightBracket,
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn literal_ended_without_content() {
        let input = b"\'";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn literal_ended_without_closing_quote() {
        let input = b"\'a";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn end_of_literal_expected() {
        let input = b"\'ab";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn literal() {
        let input = b"'a'";
        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(b'a' as CodeUnit),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn invalid_escaped_literal() {
        let input = b"'\\z'";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn escaped_literal() {
        let input = b"'\\n'";
        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(b'\n' as CodeUnit),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn hex_escaped_literal() {
        let input = b"'\\xaF'";
        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(b'\xaF' as CodeUnit),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn short_hex_excaped_literal() {
        let input = b"'\\x'";
        let cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 1;
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut output = [TokenRaw::default(); OUTPUT_SIZE];
        let cap = tokenize(input, &mut Some(&mut output));
        assert_eq!(cap, Ok(OUTPUT_SIZE));
        let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
        expected_output[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Number(0),
        };
        let expected_output = expected_output;
        assert_eq!(output, expected_output);
    }

    #[test]
    fn overlong_hex_excaped_literal() {
        if CodeUnit::MIN == 0 && core::mem::size_of::<CodeUnit>() == 4 {
            // skip this test if not u32
            let input = b"'\\xFFFFFFFFF'";
            let cap = tokenize(input, &mut None);
            assert!(cap.is_err());
        }
    }

    #[test]
    fn max_hex_excaped_literal() {
        if CodeUnit::MIN == 0 && core::mem::size_of::<CodeUnit>() == 4 {
            // skip this test if not u32
            let input = b"'\\xFFFFFFFF'";
            let cap = tokenize(input, &mut None);
            const OUTPUT_SIZE: usize = 1;
            assert_eq!(cap, Ok(OUTPUT_SIZE));
            let mut output = [TokenRaw::default(); OUTPUT_SIZE];
            let cap = tokenize(input, &mut Some(&mut output));
            assert_eq!(cap, Ok(OUTPUT_SIZE));
            let mut expected_output = [TokenRaw::default(); OUTPUT_SIZE];
            expected_output[0] = TokenRaw {
                offset: 0,
                data: TokenRawData::Number(CodeUnit::MAX),
            };
            let expected_output = expected_output;
            assert_eq!(output, expected_output);
        }
    }

    #[test]
    fn invalid_hex_excaped_literal() {
        let input = b"'\\xQ'";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn consecutive_value_from_symbol() {
        let input = b"'a' tester";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn consecutive_value_from_char_literal() {
        let input = b"5 'a'";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn consecutive_value_from_expr() {
        let input = b"5 (stuff)";
        let cap = tokenize(input, &mut None);
        assert!(cap.is_err());
    }

    #[test]
    fn invalid_symbol_typical() {
        let allowed_symbol = b"symbol";
        let allowed_symbols: &[&[u8]] = &[allowed_symbol];

        let mut raw_tokens_input = [TokenRaw::default(); 1];
        raw_tokens_input[0] = TokenRaw {
            offset: 0,
            data: TokenRawData::Symbol(b"tester"),
        };
        let raw_tokens_input = raw_tokens_input;
        let mut tokens_output = [Token::default(); 1];

        let ret = resolve_symbols(&raw_tokens_input, allowed_symbols, &mut tokens_output);
        assert!(ret.is_err());
    }

    #[test]
    fn valid_symbol_typical() {
        let allowed_symbols: &[&[u8]] = &[b"symbol0", b"symbol1", b"symbol2"];

        let mut raw_tokens_input = [TokenRaw::default(); 1];
        raw_tokens_input[0] = TokenRaw {
            offset: 15,
            data: TokenRawData::Symbol(b"symbol1"),
        };
        let raw_tokens_input = raw_tokens_input;
        let mut tokens_output = [Token::default(); 1];

        let ret = resolve_symbols(&raw_tokens_input, allowed_symbols, &mut tokens_output);
        assert!(ret.is_ok());
        assert_eq!(
            tokens_output[0],
            Token {
                offset: 15,
                data: Parsed::Symbol(1),
            }
        );
    }

    #[test]
    fn readme() {
        let input = b"123 + 12 3";
        let tokenize_cap = tokenize(input, &mut None);
        assert_eq!(tokenize_cap, Err((9, "previous token forbids value")));
    }
}

#[cfg(test)]
mod test_parsing {
    use super::*;

    #[test]
    fn basic() {
        let input = b"1 + 2";
        let tokenize_cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 3;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 2,
                output_size: 3,
            })
        );

        assert_eq!(parsed_output[0], Parsed::Number(1));
        assert_eq!(parsed_output[1], Parsed::Number(2));
        assert_eq!(parsed_output[2], Parsed::Add);
    }

    #[test]
    fn less_basic() {
        let input = b"1 * ( 3 & 4 ) + 5";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 9;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 3,
                output_size: 7,
            })
        );

        assert_eq!(parsed_output[0], Parsed::Number(1));
        assert_eq!(parsed_output[1], Parsed::Number(3));
        assert_eq!(parsed_output[2], Parsed::Number(4));
        assert_eq!(parsed_output[3], Parsed::And);
        assert_eq!(parsed_output[4], Parsed::Mul);
        assert_eq!(parsed_output[5], Parsed::Number(5));
        assert_eq!(parsed_output[6], Parsed::Add);
    }

    #[test]
    fn nested_brackets() {
        let input = b"(((((((3)))))))";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 15;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 1,
                output_size: 1,
            })
        );

        assert_eq!(parsed_output[0], Parsed::Number(3));
    }

    #[test]
    fn missing_right_bracket() {
        let input = b"( 52";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 2;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert!(parse_result.is_err());
    }

    #[test]
    fn missing_left_bracket() {
        let input = b"52 )";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 2;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert!(parse_result.is_err());
    }

    #[test]
    fn unary_op_parse() {
        let input = b"-5 * 2";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 5;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 2,
                output_size: 5,
            })
        );

        assert_eq!(parsed_output[0], Parsed::Number(0));
        assert_eq!(parsed_output[1], Parsed::Number(5));
        assert_eq!(parsed_output[2], Parsed::Sub);
        assert_eq!(parsed_output[3], Parsed::Number(2));
        assert_eq!(parsed_output[4], Parsed::Mul);
    }

    #[test]
    fn too_many_ops() {
        let input = b"1 * * 3";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 4;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(parse_result, Err((2, "stack exhausted")));
    }

    #[test]
    fn not_too_many_ops() {
        // first is binary, second is unary
        let input = b"1 + + 3";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 5;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 3,
                output_size: 5,
            })
        );

        assert_eq!(parsed_output[0], Parsed::Number(1));
        assert_eq!(parsed_output[1], Parsed::Number(0));
        assert_eq!(parsed_output[2], Parsed::Number(3));
        assert_eq!(parsed_output[3], Parsed::Add); // unary
        assert_eq!(parsed_output[4], Parsed::Add); // binary
    }

    #[test]
    fn too_many_ops_two() {
        let input = b"1 + + + 3";
        let tokenize_cap: Result<usize, (usize, &str)> = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 6;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        // output is populated
        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert!(parse_result.is_err());
    }
}

#[cfg(test)]
mod test_interpret {

    use super::*;

    #[test]
    fn interpret_overall() {
        let input = b"1 + 2";
        let tokenize_cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 3;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 2,
                output_size: 3,
            })
        );

        let mut interpret_stack = [0 as CodeUnit; 2];
        let result = interpret(&parsed_output[0..3], &mut interpret_stack, &[]);
        assert_eq!(result, 3);
    }

    #[test]
    fn interpret_complement() {
        let input = b"17 + ~17";
        let tokenize_cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 5;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 3,
                output_size: 5,
            })
        );

        let mut interpret_stack = [0 as CodeUnit; 3];
        let result = interpret(&parsed_output[0..5], &mut interpret_stack, &[]);
        assert_eq!(result, 17 + !17);
    }

    #[test]
    fn interpret_with_symbols() {
        let first_allowed_symbol = b"first";
        let second_allowed_symbol = b"second";
        let third_allowed_symbol = b"third";
        let allowed_symbols: &[&[u8]] = &[
            first_allowed_symbol,
            second_allowed_symbol,
            third_allowed_symbol,
        ];

        let symbol_values = [1, 2, 3]; // primes

        let input = b"first+second+third+5";
        let tokenize_cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 7;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, allowed_symbols, &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 2,
                output_size: 7,
            })
        );

        let mut interpret_stack = [0 as CodeUnit; 3];
        let result = interpret(&parsed_output[0..7], &mut interpret_stack, &symbol_values);
        assert_eq!(result, 1 + 2 + 3 + 5);
    }

    #[test]
    fn underflow() {
        let input = b"-5";
        let tokenize_cap = tokenize(input, &mut None);
        const OUTPUT_SIZE: usize = 3;
        assert_eq!(tokenize_cap, Ok(OUTPUT_SIZE));
        let mut tokenized_arr = [TokenRaw::default(); OUTPUT_SIZE];
        let _ = tokenize(input, &mut Some(&mut tokenized_arr));

        let mut resolved_arr = [Token::default(); OUTPUT_SIZE];
        let resolve_cap = resolve_symbols(&mut tokenized_arr, &[], &mut resolved_arr);
        assert!(resolve_cap.is_ok());

        let mut parsed_output = [Parsed::default(); OUTPUT_SIZE];
        let parse_result = parse(&mut resolved_arr, &mut parsed_output);
        assert_eq!(
            parse_result,
            Ok(ParseInfo {
                interpret_stack_size: 2,
                output_size: 3,
            })
        );

        let mut interpret_stack = [0 as CodeUnit; 2];
        let result = interpret(&parsed_output[0..3], &mut interpret_stack, &[]);
        assert_eq!(result, (0 as CodeUnit).wrapping_sub(5 as CodeUnit));
    }
}

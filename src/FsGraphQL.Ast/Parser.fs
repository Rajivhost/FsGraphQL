namespace FsGraphQL.Ast

open FParsec

/// Stores the position of a referenced error in a GraphQL document.
type Position =
    { /// Gets the line number of the position.
      Line : int64
      /// Gets the column number of the position.
      Column : int64
      /// Gets the index of the position.
      Index : int64 }

/// Stores information about a parser error.
type ParserError =
    { /// Gets the error message.
      Message : string
      /// Gets the position of the referenced error in the document.
      Position : Position }

/// Stores the result of a parsing operation.
type ParserResult =
    /// Returned in case of a successful parsing. Contains the parsed document.
    | Success of Document
    /// Returned in case of a failure at parsing. Contains the parser error information.
    | Error of ParserError

/// Exception thrown when a parsing of a GraphQL document fails.
type ParserErrorException(error : ParserError) =
    inherit exn(error.Message)
    /// Gets the position where the error is found in the Document.
    member __.Position = error.Position

/// Parser of GraphQL documents.
module Parser =
    module internal Internal =
        let ignored = 
            let whiteSpace = skipAnyOf [|'\u0009'; '\u000B'; '\u000C'; '\u0020'; '\u00A0'|]
            let lineTerminators = skipAnyOf [|'\u000A'; '\u000D'; '\u2028'; '\u2029'|]
            let comments  = pchar '#' >>. skipManyTill anyChar (lineTerminators <|>  eof) 
            let comma = skipChar ',' 
            whiteSpace <|> lineTerminators <|> comments <|> comma <?> "Ignored"

        let whiteSpaces = many ignored |>> ignore

        let token p = p .>> notFollowedBy (letter <|> digit <|> pchar '_')

        let stringToken = pstring >> token
    
        let tokenWhiteSpaces p = token p .>> whiteSpaces
    
        let stringTokenWhiteSpaces = pstring >> tokenWhiteSpaces
    
        let charTokenWhiteSpaces = pchar >> tokenWhiteSpaces

        let someOrEmpty = function | Some lst -> lst | None -> []

        let charsToString = Array.ofList >> string

        let betweenChars left right p =
            (pchar left .>> whiteSpaces) >>. p .>> (whiteSpaces .>> pchar right)

        let betweenCharsMany1 left right p =
            between (pchar left .>> whiteSpaces) (pchar right) (sepEndBy1 p whiteSpaces)

        let betweenCharsMany left right p =
            between (pchar left .>> whiteSpaces) (pchar right) (sepEndBy p whiteSpaces)

        let pairBetween seperator key value =
            (key .>> whiteSpaces) .>>. ((pchar seperator .>> whiteSpaces) >>. value)

        let inputValue, inputValueRef = createParserForwardedToRef ()

        let name = 
            let isIdentifierFirstChar c = isAsciiLetter c || c = '_'
            let isIdentifierChar c = isAsciiLetter c || isDigit c || c = '_'
            many1Satisfy2 isIdentifierFirstChar isIdentifierChar

        let stringValue =
            let escapedCharacter =
                let escaped = 
                    anyOf [| '"'; '\\'; '/'; 'b'; 'f'; 'n'; 'r'; 't' |]
                    |>> function | 'b' -> '\b' | 'f' -> '\u000C' | 'n' -> '\n' 
                                 | 'r' -> '\r' | 't' -> '\t' | c -> c 
                let unicode = 
                    pchar 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                        let hex2int c = (int c &&& 15) + (int c >>> 6)*9 
                        (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0 |> char)
                pchar '\\' >>. (escaped <|> unicode)
      
            let normalCharacter = noneOf [|'\u000A';'\u000D';'\u2028';'\u2029';'"';'\''|]
            let quote =  pchar '"'
            between quote quote (manyChars (normalCharacter <|> escapedCharacter))

        let booleanValue =
            choice [ stringToken "true" >>% true
                     stringToken "false" >>% false ]

        let integerPart = 
            let negativeSign = pchar '-'
            let nonZeroDigit = anyOf [|'1';'2';'3';'4';'5';'6';'7';'8';'9'|]
            let zero = pchar '0'
            let zeroInteger = opt negativeSign >>. zero >>% "0"
            let nonZeroInteger = 
                opt negativeSign .>>. (many1Chars2 nonZeroDigit digit) 
                |>> function | (Some _, v) -> "-" + v | (None, v) -> v
            (attempt zeroInteger) <|> nonZeroInteger

        let integerValue = integerPart |>> int64

        let floatValue =
            let exponentPart = 
                let sign = pchar '+' <|> pchar '-'
                let exponentIndicator = pchar 'e' <|> pchar 'E'
                pipe3 exponentIndicator (opt sign) (many1 digit) 
                    (fun exp sign digits ->
                        charsToString (match sign with
                                       | Some sign -> exp :: sign :: digits
                                       | None -> exp :: digits))
            let fractionPart = 
                pchar '.' .>>. (many1 digit) 
                |>> (fun (dot, digits) -> charsToString(dot::digits))
            choice [ integerPart .>>. (exponentPart <|> fractionPart) |>> fun(p1, p2)-> p1 + p2
                     pipe3 integerPart fractionPart exponentPart
                    (fun integer fraction exponent -> integer + fraction + exponent ) ] |>> float

        let nullValue = stringToken "null" >>% NullValue

        let enumValue = name

        let variable = pchar '$' >>. name 

        let objectValue =
            betweenCharsMany '{' '}' (pairBetween ':' name inputValue <?> "ObjectField") 
            |>> (List.map (fun (name, value) -> { ObjectField.Name = name; Value = value }))

        let listValue =
            betweenCharsMany '[' ']' (tokenWhiteSpaces inputValue <?> "Value") 

        inputValueRef :=
            choice [ variable |>> Variable <?> "Variable"
                     (attempt floatValue) |>> FloatValue <?> "Float"
                     integerValue |>> IntValue <?> "Integer"
                     stringValue |>> StringValue <?> "String"
                     (attempt booleanValue) |>> BooleanValue <?> "Boolean"
                     nullValue
                     enumValue |>> EnumValue  <?> "Enum"
                     objectValue |>> ObjectValue  <?> "ObjectValue"
                     listValue |>> ListValue <?> "ListValue" ]  

        let arguments = 
            let argument = 
                pairBetween ':' name inputValue
                |>> fun (name, value) -> { Argument.Name = name; Value = value } 
                <?> "Argument"
            betweenCharsMany '(' ')' argument <?> "Arguments"

        let directives = 
            let directive =
                pchar '@' >>. (name .>> whiteSpaces) .>>. (opt arguments) 
                |>> fun (name, args) -> { Directive.Name = name; Arguments = someOrEmpty args}
                <?> "Directive"
            sepEndBy directive whiteSpaces <?> "Directives"

        let inputType, inputTypeRef = createParserForwardedToRef ()
    
        let namedType = name |>> NamedType <?> "NamedType"
    
        let listType = 
            betweenChars '[' ']' inputType
            |>> ListType  <?> "ListType"
    
        let nonNullType = 
            (listType <|> namedType) .>> pchar '!' 
            |>> NonNullType <?> "NonNullType"
    
        inputTypeRef := choice [ attempt nonNullType; namedType; listType ]

        let selection, selectionRef = createParserForwardedToRef ()

        let selectionSet, selectionSetRef = createParserForwardedToRef ()

        let field = 
            let alias = tokenWhiteSpaces name .>> pchar ':' .>> whiteSpaces
            pipe5 (opt(attempt alias)) (tokenWhiteSpaces name) (opt(tokenWhiteSpaces arguments)) (opt directives) (opt selectionSet)
                (fun oalias name oargs directives oselection ->
                    (Field { Alias = oalias; Name = name; Arguments = someOrEmpty oargs;
                             Directives = someOrEmpty directives; SelectionSet = match oselection with None -> [] | Some s -> s }))
            <?> "Field"

        let selectionFragment =
            let inlineFragment =
                pipe3 (opt(stringTokenWhiteSpaces "on" >>. tokenWhiteSpaces name)) (opt(tokenWhiteSpaces directives)) selectionSet
                    (fun typeCondition directives selectionSet -> 
                        { InlineFragment.Directives = someOrEmpty directives
                          SelectionSet = selectionSet 
                          TypeCondition = typeCondition })
                |>> InlineFragment <?> "InlineFragment"
            let fragmentSpread = 
                tokenWhiteSpaces name .>>. opt directives
                |>> fun (name, directives) -> { FragmentSpread.Name = name; Directives = someOrEmpty directives }
                |>> FragmentSpread <?> "FragmentSpread"

            pstring "..." .>> whiteSpaces >>. (inlineFragment <|> fragmentSpread)  <?> "Fragment"

        selectionRef := field <|> selectionFragment  <?> "Selection"

        selectionSetRef := betweenCharsMany1 '{' '}' selection <?> "SelectionSet"

        let executableDefinitions =
            let operationType = 
                (stringTokenWhiteSpaces "query" >>% Query)
                <|> (stringTokenWhiteSpaces "mutation" >>% Mutation)
                <|> (stringTokenWhiteSpaces "subscription" >>% Subscription)
            let operationDefinition = 
                let variableDefinition =
                    pipe2 (pairBetween ':' variable inputType) (whiteSpaces >>. opt((charTokenWhiteSpaces '=') >>. inputValue))
                        (fun (variableName, variableType) defaultValue ->
                            { VariableDefinition.Name = variableName; Type = variableType
                              DefaultValue = defaultValue })
                let variableDefinitions = betweenCharsMany '(' ')' variableDefinition
                pipe5 operationType (opt(tokenWhiteSpaces name)) (opt(tokenWhiteSpaces variableDefinitions)) (opt(tokenWhiteSpaces directives)) (tokenWhiteSpaces selectionSet)
                    (fun otype name ovars directives selection ->
                        { OperationType = otype; Name = name; SelectionSet = selection
                          VariableDefinitions = someOrEmpty ovars; Directives = someOrEmpty directives })
                |>> Operation
            let shortHandQueryDefinition = selectionSet |>> QueryShorthand
            let operationDefinition = shortHandQueryDefinition <|> operationDefinition |>> OperationDefinition
            let fragmentDefinition =
                pipe4 ((stringTokenWhiteSpaces "fragment") >>. tokenWhiteSpaces name .>> (stringTokenWhiteSpaces "on")) (tokenWhiteSpaces name) directives selectionSet
                    (fun name typeCondition directives selectionSet ->
                        { Name = name;  Directives = directives; SelectionSet = selectionSet
                          TypeCondition = typeCondition })
                |>> FragmentDefinition
            sepEndBy (operationDefinition <|> fragmentDefinition) whiteSpaces

        let definitions =
            executableDefinitions |>> (List.map ExecutableDefinition)

        let document = 
            whiteSpaces >>. definitions .>> (skipMany ignored <|> eof)
            |>> (fun definitions -> { Document.Definitions = definitions })

    /// Tries to parse a GraphQL document from a string.
    let tryParse query =
        match run Internal.document query with
        | FParsec.CharParsers.ParserResult.Success (document, _, _) -> ParserResult.Success document
        | FParsec.CharParsers.ParserResult.Failure (msg, err, _) -> ParserResult.Error { Message = msg; Position = { Line = err.Position.Line; Column = err.Position.Column; Index = err.Position.Index } }

    /// <summary>
    /// Parses a GraphQL document from a string.
    /// </summary>
    /// <exception cref="GraphQLSharp.Ast.ParserErrorException">Thrown when the parser finds a error in the document.</exception>
    let parse query =
        match tryParse query with
        | ParserResult.Success document -> document
        | ParserResult.Error error -> raise (ParserErrorException(error))
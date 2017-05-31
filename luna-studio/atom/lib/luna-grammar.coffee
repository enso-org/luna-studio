{Grammar} = require "first-mate"

# https://github.com/atom/template-syntax/blob/master/stylesheets/base.less
lunaClasses = {
        ## Layout
        # BOF: '',
        # EOF: '',
        EOL: '',
        # Terminator: ''
        BlockStart: 'keyword',
        # Block: '',
        # Group: '',
        Marker: 'marker',
        ## Ident
        Var: 'variable'
        Cons: 'variable'
        Wildcard: 'keyword'
        ## Keyword
        KwAll: 'keyword'
        KwCase: 'keyword'
        KwClass: 'meta.class'
        KwDef: 'meta.class'
        KwFrom: 'keyword'
        KwImport: 'meta.import'
        KwOf: 'keyword'
        ## Operator
        Operator: 'keyword.operator'
        Modifier: 'keyword.other.special-method'
        Accessor: 'keyword.other.special-method'
        Arrow: 'keyword.operator'
        Assignment: 'keyword.operator'
        Typed: 'keyword.other.unit'
        TypeApp: 'keyword.other.unit'
        Merge: 'keyword.other.unit'
        Range: 'keyword.other.unit'
        ## Literal
        Number: 'constant.numeric'
        Quote: 'constant'
        Str: 'string'
        StrEsc: 'constant.character.escape'
        List: 'storage'
        StrWrongEsc: 'constant'
        # Separator: ''
        ## Comment
        LineComment: 'comment'
        ## Other
        # Unknown: ''
    }

module.exports =
class LunaSemanticGrammar extends Grammar
    constructor: (registry, lex) ->
        super(registry, { name: "Luna (Semantic Highlighting)"
                        , fileTypes: ["luna"]
                        , scopeName: "source.luna"
                        })
        @lex = lex

    tokenizeLine: (line, ruleStack, firstLine = false) ->
        ruleStack = 0 unless ruleStack?
        lexerLine = @lex(line)
        buffer = line
        tags = []
        tokens = []
        outerRegistry = @registry
        outerScopeName = @scopeName
        addToken = (text, lexerTags) ->
            scopes = outerScopeName
            for lexerTag in lexerTags
                cls = lunaClasses[lexerTag]
                if cls?
                    scopes += "." + cls
            tags.push outerRegistry.startIdForScope(scopes)
            tags.push text.length
            tags.push outerRegistry.endIdForScope(scopes)
            tokens.push { value: text, scopes: [scopes] }

        while buffer.length != 0
            if lexerLine.length > 0
                tokenInfo = lexerLine.shift()
                token = buffer.substr(0, tokenInfo.length)
                buffer = buffer.substr(tokenInfo.length, buffer.length)
                addToken(token, tokenInfo.tags)
            else
                addToken(buffer, [])
                buffer = ""

        return { line: line, tags: tags, tokens: tokens, ruleStack: ruleStack + 1 }

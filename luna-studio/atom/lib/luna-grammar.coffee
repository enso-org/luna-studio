{Grammar} = require "first-mate"

# https://github.com/atom/template-syntax/blob/master/stylesheets/base.less
lunaClasses = {

        # Layout      :
        # BOF         :
        # EOF         :
        # EOL         :
        # Terminator  :
        # BlockStart  :
        # Block       :
        # Group       :
        Marker      : 'marker'

        # Ident       :
        Var         : 'variable'
        Cons        : 'constant'
        Wildcard    : 'variable'

        Keyword     : 'keyword'
        KwAll       : 'keyword.control'
        KwCase      : 'keyword.control'
        KwClass     : 'meta.class'
        KwDef       : 'meta.class'
        KwImport    : 'meta.import'
        KwOf        : 'keyword.control'

        Operator    : 'keyword.operator'
        Modifier    : 'keyword.other.special-method'
        Accessor    : 'keyword.other.special-method'
        # Assignment  :
        # TypeApp     :
        # Merge       :
        # Range       :
        # Anything    :

        Literal     : 'constant'
        Number      : 'constant.numeric'
        # Quote       :
        Str         : 'string'
        StrEsc      : 'constant.character.escape'
        List        : 'storage'
        StrWrongEsc : 'invalid.illegal'

        # Control     :
        Disabled    : 'disabled'

        Comment     : 'comment'
        # Doc         :

        # Unknown     :
    }

module.exports =
class LunaSemanticGrammar extends Grammar
    constructor: (registry, lex) ->
        super(registry, { name: "Luna"
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

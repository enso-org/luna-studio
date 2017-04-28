{Grammar} = require "first-mate"

# https://github.com/atom/template-syntax/blob/master/stylesheets/base.less
lunaClasses = {
    ## Layout
    # BOF: '',
    # EOF: '',
    # EOL: '',
    # Terminator: ''
    # BlockStart: '',
    # Block: '',
    # Group: '',
    # Marker: '',
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
    Unknown: ''
  }

module.exports =
class LunaSemanticGrammar extends Grammar
  constructor: (registry, lexer) ->
    name = "Luna (Semantic Highlighting)"
    @scopeName = "source.luna"
    super(registry, {name, @scopeName})
    @lexer = lexer

  getScore: ->
    lunaGrammar = @registry.grammarForScopeName("source.luna")
    return if lunaGrammar? then (lunaGrammar.getScore.apply(lunaGrammar, arguments) + 1) else 0

  tokenizeLine: (line, ruleStack, firstLine = false) ->
    ruleStack = @lexer unless ruleStack?
    buffer = line
    tags = []
    tokens = []
    outerRegistry = @registry
    addToken = (text, lexerTags) ->
        scopes = if lexerTags.length == 0 then null else lunaClasses[lexerTags[0]] #FIXME use all keywords
        fullScopes = @scopeName + (if scopes? then ("." + scopes) else "")
        tags.push outerRegistry.startIdForScope(fullScopes)
        tags.push text.length
        tags.push outerRegistry.endIdForScope(fullScopes)
        tokens.push { value: text, scopes: [fullScopes] }

    while buffer.length != 0
        if ruleStack.length > 0
            lengthDiff = ruleStack[0].length - buffer.length

            if lengthDiff > 0
                tokenInfo = ruleStack[0]
                token = buffer
                buffer = ''
                ruleStack[0].length = lengthDiff
            else
                tokenInfo = ruleStack.shift()
                token = buffer.substr(0, tokenInfo.length)
                buffer = buffer.substr(tokenInfo.length, buffer.length)
            console.log token 
            console.log tokenInfo
            addToken(token, tokenInfo.tags)
        else
            addToken(buffer, [])
            buffer = ""

    return { line: line, tags: tags, tokens: tokens, ruleStack: ruleStack }

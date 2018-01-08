{View} = require 'atom-space-pen-views'
etch = require 'etch'
fuzzyFilter = null # defer until used
{ProjectItem, privateNewClasses, communityNewClasses, tutorialClasses} = require './project-item'
projects = require './projects'
analytics = require './gen/analytics'
report = require './report'


module.exports =
class LunaWelcomeTab extends View
    constructor: (@codeEditor) ->
        super

    @content: -> @div =>
        @div class: 'luna-welcome-scroll', =>
            @div class: 'luna-welcome-background', outlet: 'background', =>
                @div class: 'luna-welcome', outlet: 'welcomeModal', =>
                    @div class: 'luna-welcome__header', =>
                        @h1 class: 'luna-welcome__title', 'Welcome to Luna Studio'
                        @div class: 'luna-welcome__header__menu', =>
                            @input
                                class: 'luna-input luna-input--search luna-welcome-search native-key-bindings'
                                type: 'search'
                                placeholder: 'Search'
                                outlet: 'searchInput'
                            @div class: 'luna-welcome__header__menu__links', =>
                                @a
                                    class: 'luna-welcome-link luna-welcome-link--forum'
                                    href: 'http://luna-lang.org'
                                    title: 'Forum'
                                    'Forum'
                                @a
                                    class: 'luna-welcome-link luna-welcome-link--chat'
                                    href: 'http://luna-lang.org'
                                    title: 'Chat'
                                    'Chat'
                    @div class: 'luna-welcome__body', =>
                        @div class: 'luna-welcome__block luna-welcome__block--projects', =>
                            @div
                                class: 'luna-welcome__section luna-welcome__section--search-results'
                                outlet: 'searchResultsSection'
                                =>
                                    @h2 class: 'luna-welcome__section__title icon icon-search', 'Search results'
                                    @div class: 'luna-welcome__section__container', outlet: 'searchResultsContainer', =>
                            @div
                                class: 'luna-welcome__section luna-welcome__section--tutorials'
                                outlet: 'tutorialsSection'
                                =>
                                    @h2 class: 'luna-welcome__section__title icon icon-book', 'Tutorials'
                                    @div class: 'luna-welcome__section__container', outlet: 'tutorialsContainer', =>
                            @div
                                class: 'luna-welcome__section luna-welcome__section--private',
                                outlet: 'privateSection'
                                =>
                                    @h2 class: 'luna-welcome__section__title icon icon-person', 'Private'
                                    @div class: 'luna-welcome__section__container', outlet: 'privateContainer', =>
                            @div
                                class: 'luna-welcome__section luna-welcome__section--community'
                                outlet: 'communitySection'
                                =>
                                    @h2 class: 'luna-welcome__section__title icon icon-organization',  'Community'
                                    @div  class: 'luna-welcome__section__container', outlet: 'communityContainer', =>

    initialize: =>
        @tutorialItems = {}
        @privateNew = new ProjectItem({name: 'New Project', uri: null}, privateNewClasses, (progress, finalize) =>
            finalize()
            projects.temporaryProject.open())
        @communityItems = []
        @comunnityNew = new ProjectItem({name: 'New Project', uri: null}, communityNewClasses, (progress, finalize) =>
            finalize()
            report.displayError 'Not supported yet', 'Community projects are not supported yet')
        @welcomeModal.on 'click', (e) -> e.stopPropagation()
        @searchInput.on 'search', @search
        @searchInput.on 'keyup', @search
        @background.on 'click', @cancel

        projects.recent.refreshProjectsList @hideSearchResults

        @noTutorialsMsg ?= 'Fetching tutorials list...'
        @redrawTutorials()
        projects.tutorial.list (tutorial) =>
            if tutorial.error?
                @noTutorialsMsg = tutorial.error
                @redrawTutorials()
            else
                @noTutorialsMsg = ''
                item = new ProjectItem tutorial, tutorialClasses, (progress, finalize) =>
                    projects.tutorial.open tutorial, progress, finalize
                @tutorialItems[item.name] = item
                @redrawTutorials()

    redrawTutorials: =>
        @tutorialsContainer[0].innerText = @noTutorialsMsg
        for name in Object.keys @tutorialItems
            @tutorialsContainer.append(@tutorialItems[name].element)

    getFilterKey: ->
        return 'name'

    attach: (@mode) =>
        @panel ?= atom.workspace.addModalPanel({item: this, visible: false})
        @previouslyFocusedElement = document.activeElement
        @hideSearchResults()
        @panel.show()
        @searchInput.focus()
        analytics.track 'LunaStudio.Welcome.Open'

    detach: =>
        if @panel and @panel.isVisible()
            @searchInput[0].value = ''
            @panel.hide()
            @previouslyFocusedElement?.focus()
            analytics.track 'LunaStudio.Welcome.Close'
            return true
        return false

    close: =>
        if @detach()
            @onCancel = null

    cancel: =>
        if @detach()
            @onCancel?()
            @onCancel = null

    search: =>
        filterQuery = @searchInput[0].value
        if filterQuery == ""
            @hideSearchResults()
        else
            fuzzyFilter ?= require('fuzzaldrin').filter
            allItems = []
            for itemName in Object.keys @tutorialItems
                allItems.push @tutorialItems[itemName]

            allItems = allItems.concat projects.recent.getItems()
            filteredItems = fuzzyFilter(allItems, filterQuery, key: @getFilterKey())
            @showSearchResults filteredItems


    showSearchResults: (searchResults) =>
        @searchResultsContainer.empty()
        for item in searchResults
            @searchResultsContainer.append item.element

        @communitySection.hide()
        @privateSection.hide()
        @tutorialsSection.hide()
        @searchResultsSection.show()

    redrawPrivateItems: =>
        @privateContainer.empty()
        @privateContainer.append @privateNew.element
        for recentProject in projects.recent.getItems()
            @privateContainer.append recentProject.element

    redrawCommunityItems: =>
        @communityContainer.empty()
        @communityContainer.append @comunnityNew.element
        for communityItem in @communityItems
            @communityContainer.append communityItem.element

    hideSearchResults: =>
        @redrawPrivateItems()
        @redrawCommunityItems()
        @redrawTutorials()

        @searchResultsSection.hide()
        @communitySection.show()
        @privateSection.show()
        @tutorialsSection.show()

    getTitle: -> 'Welcome'

{View} = require 'atom-space-pen-views'
etch   = require 'etch'
shell  = require 'shell'
fuzzyFilter = null # defer until used
{ProjectItem, privateNewClasses, communityNewClasses} = require './project-item'
analytics = require './gen/analytics'
report = require './report'


module.exports =
class LunaWelcomeTab extends View
    constructor: (@projects) ->
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
                                @div
                                    outlet: 'docsButton'
                                    class: 'luna-welcome-link luna-welcome-link--docs'
                                    title: 'Documentation'
                                    'Documentation'
                                @div
                                    outlet: 'supportButton'
                                    class: 'luna-welcome-link luna-welcome-link--support'
                                    title: 'Community support'
                                    'Community support'
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
        @privateNew = new ProjectItem {name: 'New Project', uri: null}, privateNewClasses, (progress, finalize) =>
            finalize()
            @projects.createProject()
        @communityItems = []
        @comunnityNew = new ProjectItem({name: 'New Project', uri: null}, communityNewClasses, (progress, finalize) =>
            finalize()
            report.displayError 'Not supported yet', 'Community projects are not supported yet')
        @welcomeModal.on 'click', (e) -> e.stopPropagation()
        @searchInput.on 'search', @search
        @searchInput.on 'keyup', @search
        @background.on 'click', @cancel
        @supportButton.on 'click', -> shell.openExternal 'http://chat.luna-lang.org'
        @docsButton.on 'click', -> shell.openExternal 'http://docs.luna-lang.org'

        @projects.refreshRecentList @hideSearchResults

        @noTutorialsMsg ?= 'Fetching tutorials list...'
        @redrawTutorials()
        @projects.refreshTutorialList (error) =>
            @noTutorialsMsg = error
            @noTutorialsMsg ?= ''
            @redrawTutorials()

    redrawTutorials: =>
        @tutorialsContainer[0].innerText = @noTutorialsMsg
        @tutorialItems = @projects.getTutorialItems()
        for k, tutorialItem of @tutorialItems
            @tutorialsContainer.append(tutorialItem.element)

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
            for k, tutorialItem of @tutorialItems
                allItems.push tutorialItem

            allItems = allItems.concat @projects.getRecentItems()
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
        for recentProject in @projects.getRecentItems()
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

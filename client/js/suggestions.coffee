minFont = 12
maxFont = 25

class @suggestion
  constructor: (block, votes=0) ->
    @block = ko.observable(block)
    @votes = ko.observable(votes)

  add: ->
    log "#{@block()} with #{@votes()} votes added to list"
    window.ui.suggestions.push(@)
    window.ui.sortSuggestions()
    _.map @calcFontSize

  setVotes: (newv) ->
    @votes newv
    window.ui.sortSuggestions()

  calcFontSize: ->
      size = (maxFont - (@maxVotes() - @votes()))
      size = if size < minFont then minFont else size
      size + "px"

  maxVotes: ->
  	sug = _.max window.ui.suggestions(), (sug) -> sug.votes()
  	sug.votes()

@UI::sortSuggestions = ->
  @suggestions.sort (a,b) -> b.votes() - a.votes()

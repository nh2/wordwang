class @suggestion
  constructor: (block, votes=0) ->
    @block = block
    @votes = votes

  add: ->
    log "#{@block} with #{@votes} votes added to list"
    window.ui.suggestions.push(@)
    window.ui.sortSuggestions()

@UI::sortSuggestions = ->
  @suggestions.sort (a,b) -> b.votes - a.votes

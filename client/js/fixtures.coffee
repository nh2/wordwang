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

do ->
  new suggestion('once', 1).add()
  new suggestion('there', 3).add()
  new suggestion('was', 2).add()
  new suggestion('a', 7).add()
  new suggestion('an', 5).add()
  new suggestion('old', 10).add()
  new suggestion('man', 6).add()

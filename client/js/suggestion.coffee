class @Suggestion
  constructor: (block, votes, id=-1) ->
    @id = ko.observable id
    @block = ko.observable block
    @votes = ko.observable votes

  minFont: .6
  maxFont: 2.3

  calcFontSize: =>
    dV = window.ui.maxSuggestionVotes() - window.ui.minSuggestionVotes()
    dF = @maxFont - @minFont
    size = dF / dV * @votes() + @minFont
    size + "em"

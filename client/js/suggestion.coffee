class @Suggestion
  constructor: (block, votes, id=-1) ->
    @id = ko.observable id
    @block = ko.observable block
    @votes = ko.observable votes

  minFont: 12
  maxFont: 25

  calcFontSize: =>
    size = @maxFont - (window.ui.maxSuggestionVotes() - @votes())
    size = if size < @minFont then @minFont else size
    size + "px"

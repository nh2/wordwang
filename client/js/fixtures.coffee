suggestions1 = [
  { block: 'once', votes: 3 }
  { block: 'there', votes: 1 }
  { block: 'was', votes: 1 }
  { block: 'went', votes: 2 }
]

@UI::sortSuggestions = ->
  @suggestions = @suggestions.sort (a,b) -> b.votes - a.votes

@s1 = ->
  @ui.suggestions suggestions1

@s1()
@UI.sortSuggestions()
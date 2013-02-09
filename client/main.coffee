@log = (args...) -> console.log args...
@dir = (obj) -> console.log(JSON.stringify obj)
@assert = (bool, msg) ->
  unless bool
    throw n

class @UI
  constructor: ->

    @story = ko.observableArray []
    @suggestion = ko.observable ''
    @suggestions = ko.observableArray []

  suggest: (args...) ->
    new window.suggestion(@suggestion()).add()
    @suggestion ''

  isLastParagraph: (index) =>
    console.log index
    console.log @story().length - 1
    console.log index == @story().length - 1
    index == @story().length - 1

connectServer = ->

main = ->
  window.ui = new UI()
  ko.applyBindings(ui)

  #connectServer()

$ ->
  # Unfortunately not all browsers have window.location.origin
  origin = window.location.protocol + '//' + window.location.host

  main()

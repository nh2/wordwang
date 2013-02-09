@log = (args...) -> console.log args...
@dir = (obj) -> console.log(JSON.stringify obj)
@assert = (bool, msg) ->
  unless bool
    throw n


class @UI
  constructor: ->

    @story = ko.observable ''
    @suggestion = ko.observable ''
    @suggestions = ko.observableArray []

  suggest: (args...) ->
    s = @suggestion()
    log "suggest", s
    @suggestions.push(s)
    @suggestion ''

main = ->
  window.ui = new UI()
  ko.applyBindings(ui)


$ ->
  # Unfortunately not all browsers have window.location.origin
  origin = window.location.protocol + '//' + window.location.host

  main()

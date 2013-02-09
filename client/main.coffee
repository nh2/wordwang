@log = (args...) -> console.log args...
@dir = (obj) -> console.log(JSON.stringify obj)
@assert = (bool, msg) ->
  unless bool
    throw n


WS_URL = "ws://localhost:8888/"


class @UI
  constructor: ->

    @story = ko.observableArray []
    @suggestion = ko.observable ''
    @suggestions = ko.observableArray []

  suggest: (args...) ->
    new window.suggestion(@suggestion()).add()
    @suggestion ''

  refresh: (args) ->
    log args


connectServer = (ui) ->
  ws = new WebSocket(WS_URL, "protocolOne")

  window.debug_ping = ->
    ws.send 'ping'

  dispatcher =
    'refresh': ui.refresh

  ws.onmessage = (data) ->
    msg = JSON.parse(data.data)
    dispatch = dispatcher[msg.cmd]
    if dispatch?
      dispatch(msg.args)
    else
      throw new Error('dispatcher error: ' + msg.cmd)


main = ->
  window.ui = ui = new UI()
  ko.applyBindings(ui)

  connectServer ui

$ ->
  # Unfortunately not all browsers have window.location.origin
  origin = window.location.protocol + '//' + window.location.host

  main()

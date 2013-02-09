@log = (args...) -> console.log args...
@dir = (obj) -> console.log(JSON.stringify obj)
@assert = (bool, msg) ->
  unless bool
    throw n


WS_URL = "ws://localhost:8888/"


class @UI
  constructor: ->
    @joined = ko.observable false
    @story = ko.observableArray []
    @suggestion = ko.observable ''
    @suggestions = ko.observableArray []
    @username = ko.observable ''

  suggest: (args...) =>
    @addSuggestion(@suggestion())
    @suggestion ''

  refresh: (args) =>
    @joined true
    window.args = args

    # Assemble story
    for entry in args.groupStory
      @story.push entry.content

    for entry in args.groupCloud
      log "entry", entry
      votes = entry.cloudUids.length
      s = new suggestion(entry.cloudBlock, votes)
      s.add()

  ## Suggestions Manipulation
  addSuggestion: (block, votes = 0) =>
    s = new window.Suggestion(block, votes)
    @suggestions.push(s)
    @sortSuggestions()

  sortSuggestions: =>
    @suggestions.sort (a,b) -> b.votes() - a.votes()

  setSuggestionVotes: (sug, newv) ->
    sug.votes newv
    @sortSuggestions()

  maxSuggestionVotes: ->
    sug = _.max @suggestions(), (sug) -> sug.votes()
    sug.votes()

  joinGroup: =>
    window.send_json
      cmd: 'join'
      args:
        joinGroupId: null
        joinUserName: @username()

    false # to prevent submit

connectServer = (ui) ->
  window.ws = ws = new WebSocket(WS_URL, "protocolOne")

  window.debug_ping = ->
    ws.send 'ping'

  window.send_json = (data) ->
    console.log 'Sent', data
    ws.send JSON.stringify data

  dispatcher =
    'refresh': ui.refresh

  ws.onmessage = (data) ->
    msg = JSON.parse(data.data)
    console.log 'Received', msg
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

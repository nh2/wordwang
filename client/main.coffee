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
    setTimeout @rearrangeSuggestions, 0

  setSuggestionVotes: (sug, newv) =>
    sug.votes newv
    @sortSuggestions()

  maxSuggestionVotes: =>
    sug = _.max @suggestions(), (sug) -> sug.votes()
    sug.votes()

  joinGroup: =>
    window.send_json
      cmd: 'join'
      args:
        joinGroupId: null
        joinUserName: @username()

    false # to prevent submit

  rearrangeSuggestions: =>
    show = $ '#next ul.suggestionsShown'
    orig = $ '#next ul.suggestions:not(.suggestionsShown)'
    cont = $ 'div.story_input'
    nxt = $ '#next'
    log nxt

    # Move hidden ul which uses knockout
    oh = orig.outerHeight true
    ncss =
      top: -(cont.offset().top + oh)
      width: cont.width()
    orig.css ncss
    nxt.height oh;

    # Apply location from hidden elem to shown elem
    orig.children().each (i, sug) =>
      $sug = $ sug
      block = $("span.block", $sug).html()
      votes = $("span.votes", $sug).html()
      hash = (murmurhash3_32_gc block).toString()
      shown = $ "li.h"+hash, show
      # Fade in element if not there yet
      pos = $sug.position()
      ncss =
        top: pos.top
        left: pos.left
        width: $sug.outerWidth()
        'font-size': $sug.css 'font-size'
      if !shown.length
        ent = $sug.clone()
        ent.css ncss
        ent.addClass "h"+hash
        show.append ent
        setTimeout =>
          ent.css "opacity", 1
          , 0
      # Animate existing element otherwise
      else
        shown.css ncss
        $("span.votes", shown).html votes
        setTimeout =>
          shown.css "opacity", 1
          , 0
    #copy.remove()

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

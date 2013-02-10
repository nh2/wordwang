@log = (args...) -> console.log args...
@dir = (obj) -> console.log(JSON.stringify obj)
@assert = (bool, msg) ->
  unless bool
    throw n


UPDATE_TIME = 5000
WS_URL = "ws://#{window.location.hostname}:8888/ws"

class @UI
  constructor: ->
    @joined = ko.observable false
    @story = ko.observableArray []
    @storyParagraphs = ko.computed @paragraphs
    @suggestion = ko.observable ''
    @suggestions = ko.observableArray []
    @username = ko.observable ''
    @voting = false

  paragraphs: =>
    paragraphs = []
    paragraph = []
    for block, index in @story()
      paragraph.push block
      if index % 5 == 4
        paragraphs.push paragraph
        paragraph = []
    if paragraph.length > 0
      paragraphs.push paragraph
    paragraphs

  suggestCurrent: =>
    @suggest @suggestion()
    @suggestion ''

  refresh: (refresh_info) =>
    @joined true
    window.refresh_info = refresh_info

    # Reset all observables until we have a finer grained method
    @story []
    @suggestions []

    group = refresh_info.group

    # Assemble story
    for entry in group.groupStory
      { blockType, blockContent } = entry.content
      if blockType == 'string'
        @story.push blockContent
      else
        log "WARNING: Ingoring story entry of unknown content:", entry

    for id, entry of group.groupCloud
      votes = entry.cloudUids.length
      { blockId, content } = entry.cloudBlock
      if content.blockType == 'string'
        @addSuggestion(content.blockContent, votes, blockId)
      else
        log "WARNING: Unhandled cloud content type:", content

    @newBlock() if refresh_info.reason == "storyUpdate"
    @startVoting() if refresh_info.reason == "cloudUpdate" and not @voting

    @sortSuggestions()

  # Sends suggestion to the server
  suggest: (block) =>
    if block == ""
      return
    s = _.find @suggestions(), (sug) =>
      sug.block() == block
    if s?
      log "Upvoting id #{s.id}"
      @server 'upvote', s.id
    else
      log "Sending block #{block}"
      @server 'send',
        blockType: 'string'
        blockContent: block

  # Directly add suggestion to the UI
  addSuggestion: (block, votes=0, blockId=-1) =>
    s = new window.Suggestion(block, votes, blockId)
    log 'displaying suggestion', '"'+s.block()+'"', 'with', s.votes(), 'votes'
    @suggestions.push s
    # @sortSuggestions()

  sortSuggestions: =>
    if @suggestions().length is 0
      @clearSuggestions()
    else
      @suggestions.sort (a,b) -> b.votes() - a.votes()
      setTimeout @rearrangeSuggestions, 0

  setSuggestionVotes: (sug, newv) =>
    sug.votes newv
    @sortSuggestions()

  minSuggestionVotes: =>
    sug = _.min @suggestions(), (sug) -> sug.votes()
    sug.votes()

  maxSuggestionVotes: =>
    sug = _.max @suggestions(), (sug) -> sug.votes()
    sug.votes()

  joinGroup: =>
    @server 'join',
      joinGroupId: null
      joinUserName: @username()

  server: (cmd, args) =>
    o =
      cmd: cmd
      args: args
    log '-> server: ', o
    window.send_json o

  # Speak the story using Google Translate
  speak: =>
    escapedStory = encodeURIComponent(@story().join(' '))
    gtUrl = "http://translate.google.com/translate_tts?ie=UTF-8&q=#{escapedStory}&tl=en&total=1&idx=0"
    log "adding audio to #{gtUrl}"
    $("<audio class='tts' autoplay='true'><source src='#{gtUrl}' type='audio/mpeg'; codecs='mp3'></audio>").appendTo("body")

  clearSuggestions: =>
    show = $ '#next ul.suggestionsShown'
    nxt = $ '#next'
    show.html ''
    nxt.height 0

  startVoting: =>
    @voting = true
    $('#next-box')
      .animate { 'progress': '100%' }, # bit of a hack
        duration: UPDATE_TIME
        easing: 'linear'
        step: (now) -> $(@).css 'background-size', "#{now}% 100%"


  newBlock: =>
    @voting = false
    $('#next-box')
      .stop()
      .css('background-size', '0% 100%')

  rearrangeSuggestions: =>
    show = $ '#next ul.suggestionsShown'
    orig = $ '#next ul.suggestions:not(.suggestionsShown)'
    cont = $ 'div.story_input'
    nxt = $ '#next'

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
        ent.click =>
          $sug.click()
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
    console.log '<- server', msg
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

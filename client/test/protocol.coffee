{ chai, intersect } = window
{ expect } = chai
chai.should()


WS_URL = "ws://localhost:8888/"

with_connect = (f, onmessage) ->
  ws = new WebSocket(WS_URL, "protocolOne")
  ws.onopen = ->
    f ws

onmessage = (ws, f) ->
  ws.onmessage = (msg) ->
    f JSON.parse(msg.data)


@debug =
  connect: -> with_connect (ws) -> window.ws = ws


describe 'connectivity', ->

  it "should connect to websocket", (done) ->

    with_connect (ws) ->
      done()


describe 'protocol', ->

  it 'ping delivers correct structure', (done) ->

    with_connect (ws) ->

      ws.send 'ping'

      onmessage ws, (msg) ->
        console.log msg
        msg.cmd.should.equals 'refresh'

        { groupCloud, groupId, groupStory, groupUsers } = msg.args

        parseInt(groupId).should.be.a 'number'

        jsschema.check(schemas.refresh, msg)

        done()

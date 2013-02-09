{ chai, intersect } = window
{ expect } = chai
chai.should()


describe 'connectivity', ->

  it "should connect to websocket", (done) ->

    ws = new WebSocket("ws://localhost:8888/", "protocolOne")

    ws.onopen = ->
      done()

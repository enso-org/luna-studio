require "babel-core/register"
require "babel-polyfill"
import * as basegl from 'basegl'

import {Connection}      from 'view/Connection'
import {ExpressionNode}  from 'view/ExpressionNode'
import {InputNode}       from 'view/InputNode'
import {NodeEditor}      from 'view/NodeEditor'
import {OutputNode}      from 'view/OutputNode'
import {Searcher}        from 'view/Searcher'
import {subscribeEvents} from 'view/Component'

export install = (name, f) ->
    scene = basegl.scene {domElement: name}
    basegl.fontManager.register 'DejaVuSansMono', 'rsc/DejaVuSansMono.ttf'
    await basegl.fontManager.load 'DejaVuSansMono',
        glyphSize: 20
        spread: 32

    nodeEditor = new NodeEditor scene
    nodeEditor.initialize()
    f nodeEditor

export onEvent = subscribeEvents

main = (f) -> install 'basegl-root', f

window.run = main

runExample = -> main (nodeEditor) ->
    nodeEditor.setNodes [
        new ExpressionNode
            key: 1
            name: "foo"
            inPorts: [{key: 1}]
            outPorts: [{key: 1}
                      ,{key: 2}]
            position: [200, 300]
            expanded: false
            selected: false
        new ExpressionNode
            key: 2
            name: "bar"
            inPorts: [{key: 1}
                     ,{key: 2}
                     ,{key: 3}
                     ,{key: 4}
                     ]
            outPorts: [{key: 1}]
            position: [200, 600]
            expanded: false
            selected: false
        new ExpressionNode
            key: 3
            name: "foo bar baz"
            inPorts: [{key: 1}
                     ,{key: 2}]
            outPorts: [{key: 1}]
            position: [500, 300]
            expanded: false
            selected: false
        new ExpressionNode
            key: 4
            name: "node1"
            inPorts: [{key: 1}]
            outPorts: [{key: 1}]
            position: [500, 600]
            expanded: false
            selected: false
        ]

    nodeEditor.setInputNode new InputNode
        key: 'in'
        outPorts: [ {key: 1}
                  , {key: 2}
                  , {key: 3}]
    nodeEditor.setOutputNode new OutputNode
        key: 'out'
        inPorts: [ {key: 1}
                 , {key: 2}]
    nodeEditor.setConnections [
        new Connection
            key: 0
            srcNode: 'in'
            srcPort: 2
            dstNode: 1
            dstPort: 1
        new Connection
            key: 1
            srcNode: 1
            srcPort: 1
            dstNode: 2
            dstPort: 1
        new Connection
            key: 2
            srcNode: 2
            srcPort: 1
            dstNode: 3
            dstPort: 1
        new Connection
            key: 3
            srcNode: 3
            srcPort: 1
            dstNode: 4
            dstPort: 1
        new Connection
            key: 4
            srcNode: 4
            srcPort: 1
            dstNode: 'out'
            dstPort: 2
        ]
    nodeEditor.setSearcher new Searcher
        key: 4
        mode: 'node'
        matches: [
            name: 'bar'
            doc:  'bar description'
            mode: 'class'
        ,
            name: 'foo'
            doc:  'foo multiline\ndescription'
            mode: 'function'
        ]


    subscribeEvents (path, event) =>
        console.warn {path: path, base: event}
    window.n = nodeEditor

if NODE_EDITOR_EXAMPLE? then runExample()
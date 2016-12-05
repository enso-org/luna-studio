$$         = require('common')
colors     = require('colors')
vs         = require('shaders/sdf.vert')()
fs         = require('shaders/port.frag')()
fs_self    = require('shaders/port_self.frag')()

nodeRadius = 25.0
colorFar   = colors[2]
halfWidth  = width / 2.0
margin     = 0.0
dist       = nodeRadius + halfWidth + margin
nodeSize   = 30.0

height     = 80.0
width      = 80.0

class Port
  constructor: (widgetId, isSelf) ->
    @isSelf = isSelf
    @uniforms =
      size:      {type: 'v2', value: new THREE.Vector2(width, height)}
      color:     {type: 'v4', value: new (THREE.Color)('red')}
      colorFar:  {type: 'v4', value: colorFar}
      highlight: {type: 'i',  value: 0}
      mouseDist: {type: 'f',  value: 0.0}
      nodeSize:  {type: 'f',  value: nodeSize}
      portSize:  {type: 'f',  value: height}
      portCount: {type: 'i',  value: 1}
      onlyPort:  {type: 'i',  value: 0}
      angle:     {type: 'f',  value: 1}
      objectId:  {type: 'v3', value: new (THREE.Vector3)(widgetId % 256 / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0)}

    @uniforms[k] = v for k, v of $$.commonUniforms

    @mesh = new (THREE.Mesh)(new (THREE.PlaneBufferGeometry)(width, height), new (THREE.ShaderMaterial)(
      uniforms: @uniforms
      vertexShader: vs
      fragmentShader: if isSelf then fs_self else fs
      transparent: true
      blending: THREE.NormalBlending
      side: THREE.DoubleSide
      extensions:
        derivatives: true
      ))

    @setAngle 0

  setAngle: (angle, count, only) ->
    @uniforms.angle.value     = angle
    @uniforms.portCount.value = count
    @uniforms.onlyPort.value  = if only then 1 else 0

  setColor:        (color)     -> @uniforms.color.value     = colors[color]
  setHighlight:    (val)       -> @uniforms.highlight.value = if val then 1 else 0
  updateMouseDist: (mouseDist) -> @uniforms.mouseDist.value = mouseDist
  widgetMoved: -> null
module.exports = Port

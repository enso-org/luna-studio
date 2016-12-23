$$           = require('common')
vs           = require('shaders/sdf.vert')()

LabeledWidget = require('Widget/LabeledWidget')

class Toggle extends LabeledWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height, true
    @createIndicator()
    @relayout()

  createIndicator: ->
    @uniforms =
      size:      { type: 'v2', value: new THREE.Vector2(@width, @height) }
      value:     { type: 'i',  value: 0 }

    @uniforms[k] = v for k, v of $$.commonUniforms

    bgMesh = new (THREE.PlaneBufferGeometry)(1, 1)
    bgMesh.applyMatrix (new (THREE.Matrix4)).makeTranslation(0.5, 0.5, 0.0)

    @indicator = new (THREE.Mesh)(bgMesh, new (THREE.ShaderMaterial)(
      uniforms: @uniforms
      vertexShader: vs
      fragmentShader: require('shaders/toggle.frag')()
      transparent: true
      blending: THREE.NormalBlending
      side: THREE.DoubleSide
      extensions:
        derivatives:    true
      ))

    @mesh.add @indicator

  setValue: (value) -> @uniforms.value.value = if value then 1 else 0

  relayout: ->
    super

    @indicator.position.x  = @width  * 0.8 - (@height / 2.0)
    @indicator.position.y  = @height * 0.1

    @uniforms.size.value.x = @indicator.scale.x = @width  * 0.2
    @uniforms.size.value.y = @indicator.scale.y = @height * 0.8

module.exports = Toggle

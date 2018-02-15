/** @babel */
/** @jsx etch.dom */
const $ = require('jquery');
var Spinner;

etch = require('etch');

module.exports = Spinner = class Spinner {
    constructor(progress = null, overlap = false) {
        this.overlap = overlap;
        this.update = this.update.bind(this);
        this.render = this.render.bind(this);

        this.dashArray = 538.97948963149890246986;
        this.dashOffset = 0;
        this.opacity = 1;
        this.started = false;

        this.lastProgress = null;
        if(progress != null) {
            this.setProgress(progress);
        }
        etch.initialize(this);
    }

    update(props, children) {
        return etch.update(this);
    }

    render() {
        if(this.overlap)
            return  <div id="luna_logo-spinner" class="luna_logo-overlap">
                      { this.renderSpinner() }
                    </div>
        else
            return  <div id="luna_logo-spinner">
                      { this.renderSpinner() }
                    </div>

    }
    renderSpinner () {
        return <div id="logo-area">
                  <div id="spinner" class={ this.started ? "rotating" : "" }>
                      <svg id="install-spinner" height="180" width="180" class="spinner__circle" shape-rendering="geometricPrecision" viewBox="0 0 180 180">
                          <circle class="logo-circle" id="progress-bg" r="85.78125" cx="90" cy="90" fill="transparent" style="stroke: rgba(127, 127, 127, 0.2);"></circle>
                          <circle class="logo-circle" id="progress-bar" r="85.78125" cx="90" cy="90" fill="transparent"  style={{stroke: "rgba(255, 255, 255, 0.2)", "stroke-dashoffset": this.dashOffset, "stroke-dasharray": this.dashArray}}>
                          </circle>
                          <svg id="full-ring" opacity={this.opacity}>
                              <circle id="full-ring-bg" r="85.78125" cx="90" cy="90" fill="transparent" ></circle>
                              <circle class="logo-circle" r="85.78125" cx="90" cy="90" fill="transparent" style="stroke: rgba(255, 255, 255, 0.2)"></circle>
                          </svg>
                      </svg>
                  </div>
              </div>
    }

    start() {
        this.opacity = 0;
        this.started = true;
    }
    setProgress(progress) {
        if(!this.started)
            this.start();
        if(!this.lastProgress || this.lastProgress < progress) {
            var value = (0.1 + progress)/1.1;
            var dashValue = this.dashArray;
            this.dashOffset = dashValue - value * dashValue;
            this.lastProgress = progress;
        }
    }
    finish() {
        this.opacity = 1;
        this.started = false;
    }
};

/** @babel */
/** @jsx etch.dom */
const Spinner = require('./spinner');

var ProjectItem;

etch = require('etch');
path = require('path');

projectClasses = "luna-welcome__tile ";

module.exports = {
    ProjectItem: class ProjectItem {
        constructor(project, classes = "", onOpen) {
            this.update = this.update.bind(this);
            this.render = this.render.bind(this);
            this.thumbLogo = this.thumbLogo.bind(this);
            this.uri = project.uri;
            this.name = project.name != undefined ? project.name :
                (project.uri != undefined ? path.basename(project.uri) : '(Unnamed)');
            this.description = project.description != undefined ? project.description : "";
            this.classes = classes;
            this.onOpen = function() {
                spinner = new Spinner(progress = 0)
                var self = this;
                update = function() {
                    self.update( { logo: spinner.render() } );
                }
                progress = function(p) {
                    spinner.setProgress(p);
                    update();
                };
                finalize = function() {
                    self.update( { logo: self.thumbLogo() } );
                }
                update();
                onOpen(progress, finalize);
            };
            this.thumb = project.thumb != undefined ? project.thumb : "atom://luna-studio/rsc/folder.svg";
            this.props = { logo: this.thumbLogo() }
            etch.initialize(this);
        }

        update(props, children) {
            this.props = props;
            return etch.update(this);
        }
        thumbLogo() {
            return <img class="luna-project-logo" src={this.thumb}></img>
        }
        render () {
            return  <div class={this.classes} on={{click: this.onOpen}}>
                        {this.props.logo}
                        <div class="luna-project-caption">{this.name}</div>
                        <div class="luna-project-description">{this.description}</div>
                    </div>
        }
    },
    sampleProjectClasses: projectClasses + "luna-welcome__tile--sample-project",
    recentClasses: projectClasses + "luna-welcome__tile--recent",
    privateNewClasses: projectClasses + 'luna-welcome__tile--add-new',
};

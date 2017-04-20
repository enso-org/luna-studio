# Flowbox Data changelog

The website http://flowbox.io/data/preview redirects always to the newest version!

## v 0.1 | 03.06.2015
* JavaScript preview of nodes (dragging)
* Proof of the concept for Haskell <-> JS bindings

## v 0.2 | [26.06.2015](http://flowbox.io/data/preview/2015-06-27/)

* New Haskell based reactive framework
  - It allows for future faster GUI development and proper event handling

* Completely new node editor
  - Haskell / JavaScript and WebGL based
  - Displaying nodes with new shaders
  - Selecting nodes
  - Toggle selection (holding alt)
  - Focus node - remembering selection history and last selected node
  - Moving nodes
  - Pan / zoom (right and middle mouse button, arrows and +/-)

## v 0.3 | [13.07.2015](http://flowbox.io/data/preview/2015-07-13/)

* Node editor development:
 - Create new nodes with new Node Searcher (Tab key)
 - Select multiple nodes by dragging
 - Move multiple nodes at any panning and zoom level
 - Remove focused node ('r')
 - Display text (node labels) in WebGL
 - Reset zoom ('z')
 - Select all nodes (shift 'a')
 - Unselect nodes (Esc)
 - Nice looking antialiasing of nodes at all zoom levels
 - Basic port shaders
* Under the hood
 - Major rework of GUI framework
 - Almost all JavaScript part converted to Haskell
 - Performance optimization
 - Feature switches in GUI aiming at Continous Delivery

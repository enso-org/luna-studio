{app, BrowserWindow} = require('electron')

######################
### Error handling ###
######################

errMsg = (action) ->
  'Luna Studio process ' + action + 
    '. Please report the error to http://github.com/luna/luna-studio/issues.'

addCrashHandler = (win) ->
  win.webContents.on 'crashed', () ->
    options =
      type    : 'info'
      title   : 'Luna Studio Process Crash.'
      message : errMsg 'crashed'
      buttons : ['Reload', 'Close']
    
    dialog.showMessageBox options, (index) ->
      if index == 0
        win.reload()
      else 
        win.close()

addHangHandler = (win) ->
  win.on 'unresponsive', () ->
    options =
      type: 'info'
      title: 'Luna Studio Process Hanging'
      message: errMsg 'is hanging'
      buttons: ['Reload', 'Close']
    
    dialog.showMessageBox options, (index) ->
      if index == 0
        win.reload() 
      else 
        win.close()



#######################
### Window creation ###
#######################

# Global reference in order to prevent garbage collecting
mainWindow = null

createWindow = () ->  
  mainWindow = new BrowserWindow
    width       : 800
    height      : 600
    frame       : false
    transparent : true

  mainWindow.on 'closed', () -> mainWindow = null
  addCrashHandler mainWindow
  addHangHandler  mainWindow

  dir = __dirname
  address = 'file://' + dir + '/index.html'
  mainWindow.loadURL address
  

app.on 'ready', createWindow

app.on 'window-all-closed', () ->
  if (process.platform != 'darwin')
    app.quit()

app.on 'activate', () ->
  if (mainWindow == null)
    createWindow()
    

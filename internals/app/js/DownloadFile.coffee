downloadFile = do ->
  a = document.createElement('a')
  document.body.appendChild a
  a.style = 'display: none'
  (data, fileName) ->
    blob = new Blob([ data ], type: 'octet/stream')
    url = window.URL.createObjectURL(blob)
    a.href = url
    a.download = fileName
    a.click()
    window.URL.revokeObjectURL url

module.exports =
  downloadFile: downloadFile

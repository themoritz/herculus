var React = require('react')
var ReactVR = require('react-virtualized')

module.exports = React.createClass({
  render: function () {
    var self = this
    return React.createElement(ReactVR.AutoSizer, {}, function (arg) {
      return React.createElement(ReactVR.Grid, {
        cellRenderer: self.props.cellRenderer,
        columnCount: self.props.columnCount,
        columnWidth: function (arg) {
          return self.props.columnWidths[arg.index]
        },
        height: arg.height,
        width: arg.width,
        rowCount: self.props.rowCount,
        rowHeight: function (arg) {
          return self.props.rowHeights[arg.index]
        }
      })
    })
  }
})

var React = require('react')
var ReactVR = require('react-virtualized')

module.exports = React.createClass({
  render: function () {
    var self = this
    return React.createElement(ReactVR.AutoSizer, {}, function (arg) {
      return React.createElement(ReactVR.Grid, {
        cellRenderer: self.props.cellRenderer,
        columnCount: self.props.columnCount,
        columnWidth: self.props.columnWidth,
        height: arg.height,
        width: arg.width,
        rowCount: self.props.rowCount,
        rowHeight: self.props.rowHeight
      })
    })
  }
})

var React = require('react')
var ReactVR = require('react-virtualized')

module.exports = React.createClass({
  render: function () {
    return React.createElement(ReactVR.AutoSizer, {}, this.props.renderChildren)
  }
})

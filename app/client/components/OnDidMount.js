var React = require('react')

module.exports = React.createClass({
  componentDidMount: function () {
    return this.props.onDidMount()
  },
  render: function () {
    return this.props.children
  }
})

var React = require('react');
var onClickOutside = require('react-onclickoutside')

module.exports = React.createClass({
  mixins: [
    onClickOutside
  ],
  render: function () {
    return this.props.children
  },
  handleClickOutside: function (e) {
    return this.props.onClickOutside()
  }
})

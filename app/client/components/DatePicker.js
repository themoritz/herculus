var React = require('react');
var DatePicker = require('react-datepicker');
var moment = require('moment');

module.exports = React.createClass({
  render: function() {
    var self = this
    return React.createElement(DatePicker, {
      dateFormat: self.props.dateFormat,
      placeholderText: self.props.placeholderText,
      selected: moment(self.props.selected),
      showYearDropdown: true,
      onChange: function (d) {
        return self.props.onChange(d.format(self.props.dateFormat))
      }
    })
  }
})

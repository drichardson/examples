// ==========================================================================
// CustomView.MyTableView
// ==========================================================================

require('core');

/** @class

  (Document Your View Here)

  @extends SC.View
  @author AuthorName
  @version 0.1
*/
CustomView.MyTableView = SC.View.extend(
/** @scope CustomView.MyTableView.prototype */ {

  emptyElement: '<table><tr><td></td></tr></table>',
  content: [],
  contentBindingDefault: SC.Binding.MultipleNotEmpty,
  
  render: function() {
      var html = [];
      var content = this.get('content');
      
      // Iterate through the collection and add rows
      html.push(this._renderRowHtml(content));
      
      // Finally set the innerHTML of the view
      html = html.join('');
      this.set('innerHTML', html);
  }.observes('content'),
  
  _renderRowHtml: function(content) {
      var html = [];
      
      content.each( function(record) {
          html.push('<tr>');
          
          var noColumns = record.get('properties').length;
          for(i=0; i < noColumns; i++){
              html.push('<td style="border: 1px solid #fff;">');
              html.push(record.get(record.get('properties')[i]));
              html.push('</td>');
          };
          
          html.push('</tr>');
      });
      return html.join('');
  }
}) ;

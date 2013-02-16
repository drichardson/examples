// ==========================================================================
// CustomView.Products
// ==========================================================================

require('core');

/** @class

  (Document your class here)

  @extends SC.Record
  @author AuthorName
  @version 0.1
*/
CustomView.Products = SC.Record.extend(
/** @scope CustomView.Products.prototype */ {

  dataSource:  CustomView.server,
  properties: ['name', ' manufacturer', 'price']

}) ;

// ==========================================================================
// Project:   Lethe.Channel
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals Lethe */

/** @class

  (Document your Model here)

  @extends SC.Record
  @version 0.1
*/
Lethe.Channel = SC.Record.extend(
    /** 
     * @scope Lethe.Channel.prototype 
     */ 
    {
        name: SC.Record.attr(String),

        peers: [],
        messages: []

    }
);

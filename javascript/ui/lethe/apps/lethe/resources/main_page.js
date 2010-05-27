/**
 * Copyright (c) dushin.net
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of dushin.net nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*globals Lethe */

Lethe.mainPage = SC.Page.design(
    {
        
        // The main pane is made visible on screen as soon as your app is loaded.
        // Add childViews to this pane for views to display immediately on page 
        // load.
        mainPane: SC.MainPane.design(
            {
                /*
                childViews: 'labelView'.w(),
                labelView: SC.LabelView.design(
                    {
                        layout: { centerX: 0, centerY: 0, width: 200, height: 18 },
                        textAlign: SC.ALIGN_CENTER,
                        tagName: "h1", 
                        value: "Welcome to SproutCore!"
                    }
                )
                */
                childViews: 'topView middleView bottomView'.w(),
                topView: SC.ToolbarView.design(
                    {
                        anchorLocation: SC.ANCHOR_TOP,
                        layout: { top: 0, left: 0, right: 0, height: 24 },
                        childViews: 'labelView addButton'.w(),
                        labelView: SC.LabelView.design(
                            {
                                layout: { centerY: 0, height: 24, left: 8, width: 200 },
                                controlSize: SC.LARGE_CONTROL_SIZE,
                                fontWeight: SC.BOLD_WEIGHT,
                                value: 'Lethe'
                            }
                        ),
                        addButton: SC.ButtonView.design(
                            {
                                layout: { centerY: 0, height: 24, right: 12, width: 100 },
                                title: "Add Task"
                            }
                        )
                    }
                ),
                middleView: SC.ScrollView.design(
                    {
                        hasHorizontalScroller: NO,
                        layout: { top: 36, bottom: 32, left: 0, right: 0 },
                        backgroundColor: 'white',
                        contentView: SC.ListView.design(
                            /*
                            {
                                contentBinding: 'Todos.tasksController.arrangedObjects',
                                selectionBinding: 'Todos.tasksController.selection',
                                contentValueKey: "description",
                                contentCheckboxKey: "isDone",
                                rowHeight: 21
                            }
                            */
                        )
                    }
                ),
                bottomView: SC.ToolbarView.design(
                    {
                        layout: { bottom: 0, left: 0, right: 0, height: 32 },
                        anchorLocation: SC.ANCHOR_BOTTOM,
                        childViews: 'summaryView'.w(),
                        summaryView: SC.LabelView.design(
                            {
                                layout: { centerY: 0, height: 18, left: 20, right: 20 },
                                textAlign: SC.ALIGN_CENTER,
                                // valueBinding: "Todos.tasksController.summary"
                            }
                        )
                    }
                )
            }
        )
    }
);

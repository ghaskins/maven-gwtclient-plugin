package com.novell.lsg.gwtclient.example.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;

/**
 * This class is the main entry point for our GWT module.
 */
public class Index implements EntryPoint
{
    @Override
    public void onModuleLoad()
    {        
        FlowPanel panel = new FlowPanel();
        
        panel.add(new Label("Hello, World"));
        
        RootPanel.get("mainContent").add(panel);
    }
}

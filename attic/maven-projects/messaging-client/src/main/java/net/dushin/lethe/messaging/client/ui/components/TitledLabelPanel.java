package net.dushin.lethe.messaging.client.ui.components;

public class TitledLabelPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = 4797999500355727852L;
    private final javax.swing.JLabel textPanel;
    
    TitledLabelPanel(
        final String title
    ) {
        this(title, "");
    }
    
    
    TitledLabelPanel(
        final String title,
        final String text
    ) {
        final javax.swing.JLabel titlePanel = new javax.swing.JLabel(title);
        textPanel = new javax.swing.JLabel(text);
        titlePanel.setLabelFor(textPanel);
        
        this.setLayout(new java.awt.BorderLayout());
        add("West", titlePanel);
        add("Center", textPanel);
    }
    
    javax.swing.JLabel
    getTextLabel() {
        return this.textPanel;
    }
}

package net.dushin.lethe.messaging.server;

import javax.xml.ws.Endpoint;

import net.dushin.lethe.messaging.interfaces.Constants;
import net.dushin.lethe.messaging.server.config.MessagingServerConfigType;

import org.apache.cxf.Bus;
import org.apache.cxf.buslifecycle.BusLifeCycleListener;
import org.apache.cxf.buslifecycle.BusLifeCycleManager;

public class MessengerBusLifecycleListener implements BusLifeCycleListener {
    
    private final MessagingServerConfigType serverConfig;
    
    private Endpoint endpoint;
    
    MessengerBusLifecycleListener(
        final Bus bus,
        final MessagingServerConfigType serverConfig
    ) {
        final BusLifeCycleManager mgr = bus.getExtension(BusLifeCycleManager.class);
        if (mgr == null) {
            throw new IllegalStateException("Unable to resolve the BusLifeCycleManager Bus extension");
        }
        mgr.registerLifeCycleListener(this);
        this.serverConfig = serverConfig;
    }
    
    public void initComplete() {
        final Messenger messenger = new Messenger(serverConfig);
        endpoint = Endpoint.create(messenger);
        final java.util.Map<String, Object> properties = 
            new java.util.TreeMap<String, Object>();
        properties.put(javax.xml.ws.Endpoint.WSDL_SERVICE, this.serverConfig.getServiceQName());
        properties.put(javax.xml.ws.Endpoint.WSDL_PORT, Constants.MESSAGE_PORT_QNAME);
        endpoint.setProperties(properties);
        endpoint.publish(
            "http://" 
            + this.serverConfig.getHost() 
            + ":"
            + this.serverConfig.getPort()
            + '/'
            + this.serverConfig.getUrlContext()
        );
    }

    public void preShutdown() {
        endpoint.stop();
    }

    public void postShutdown() {
        // complete
    }
}

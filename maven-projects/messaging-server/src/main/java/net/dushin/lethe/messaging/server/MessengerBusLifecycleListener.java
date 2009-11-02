package net.dushin.lethe.messaging.server;

import javax.xml.ws.Endpoint;

import net.dushin.lethe.messaging.interfaces.Constants;
import net.dushin.lethe.messaging.server.config.EndpointConfigListType;
import net.dushin.lethe.messaging.server.config.EndpointConfigType;
import net.dushin.lethe.messaging.server.config.MessagingServerConfigType;

import org.apache.cxf.Bus;
import org.apache.cxf.buslifecycle.BusLifeCycleListener;
import org.apache.cxf.buslifecycle.BusLifeCycleManager;

public class MessengerBusLifecycleListener implements BusLifeCycleListener {
    

    private final MessagingServerConfigType serverConfig;
    
    private java.util.List<Endpoint> endpoints = new java.util.ArrayList<Endpoint>();
    
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
        final EndpointConfigListType cfgs = EndpointUtil.getEndpointConfigs(this.serverConfig);
        for (EndpointConfigType cfg : cfgs.getEndpointConfig()) {
            endpoints.add(createAndPublishEndpoint(messenger, cfg));
        }
    }

    public void preShutdown() {
        for (final Endpoint endpoint : endpoints) {
            endpoint.stop();
        }
    }

    public void postShutdown() {
        // complete
    }
    
    private static Endpoint
    createAndPublishEndpoint(
        final Messenger messenger,
        final EndpointConfigType cfg
    ) {
        final Endpoint endpoint = Endpoint.create(messenger);
        final java.util.Map<String, Object> properties = 
            new java.util.TreeMap<String, Object>();
        properties.put(javax.xml.ws.Endpoint.WSDL_SERVICE, cfg.getServiceQName());
        properties.put(javax.xml.ws.Endpoint.WSDL_PORT, Constants.MESSAGE_PORT_QNAME);
        endpoint.setProperties(properties);
        endpoint.publish(
            "http://" 
            + cfg.getHost() 
            + ":"
            + cfg.getPort()
            + '/'
            + cfg.getUrlContext()
        );
        return endpoint;
    }
}

package net.dushin.lethe.messaging.server;

import net.dushin.lethe.messaging.server.config.EndpointConfigListType;
import net.dushin.lethe.messaging.server.config.EndpointConfigType;
import net.dushin.lethe.messaging.server.config.MessagingServerConfigType;

final class EndpointUtil {

    private static final EndpointConfigType DEFAULT_ENDPOINT_CONFIG = 
        new EndpointConfigType();
    
    private
    EndpointUtil() {
        // complete
    }


    static EndpointConfigListType
    getEndpointConfigs(final MessagingServerConfigType serverConfig) {
        EndpointConfigListType ret = serverConfig.getEndpointConfigList();
        if (ret == null) {
            ret = new EndpointConfigListType();
            ret.getEndpointConfig().add(DEFAULT_ENDPOINT_CONFIG);
        }
        return ret;
    }
}

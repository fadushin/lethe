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
package net.dushin.lethe.messaging.server.spring;

import net.dushin.lethe.messaging.server.Messenger;
import net.dushin.lethe.messaging.server.config.MessagingServerConfigType;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.xml.AbstractSingleBeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;

public class MessagingServerConfigBeanDefinitionParser 
    extends AbstractSingleBeanDefinitionParser {

    @Override
    protected void 
    doParse(
        final org.w3c.dom.Element element, 
        final ParserContext ctx, 
        final BeanDefinitionBuilder bean
    ) {
        bean.addConstructorArg(parseMessagingServerConfig(element));
    }

    @Override
    protected Class 
    getBeanClass(
        final org.w3c.dom.Element e
    ) {
        return Messenger.class;
    }
    
    private static MessagingServerConfigType
    parseMessagingServerConfig(
        final org.w3c.dom.Element element
    ) {
        return deserialize(
            MessagingServerConfigType.class.getPackage(),
            element,
            MessagingServerConfigType.class
        );
    }
    
    /**
     * Deserialize a serialized structure from a serialized structure.
     */
    private static <T> T
    deserialize(
        final Package pkg,
        final org.w3c.dom.Element element,
        final Class<T> type
    ) {
        try {
            final javax.xml.bind.JAXBContext ctx =
                javax.xml.bind.JAXBContext.newInstance(
                    pkg.getName()
                );
            final javax.xml.bind.Unmarshaller unmarshaller = ctx.createUnmarshaller();
            final javax.xml.bind.JAXBElement<T> jaxb = unmarshaller.unmarshal(
                element, 
                type
            );
            return jaxb.getValue();
        } catch (final javax.xml.bind.JAXBException e) {
            throw new RuntimeException("Error unmarshalling " + element.getLocalName(), e);
        }
    }
    
}

package telraam;

import java.io.IOException;
import java.util.Locale;
import java.util.concurrent.CountDownLatch;
import java.util.logging.Logger;

import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.StatusCode;
import org.eclipse.jetty.websocket.api.WebSocketAdapter;

public class WebSocketHandler extends WebSocketAdapter
{
    private static final Logger logger = Logger.getLogger(WebSocketHandler.class.getName());
    private final CountDownLatch closureLatch = new CountDownLatch(1);

    @Override
    public void onWebSocketConnect(Session sess)
    {
        super.onWebSocketConnect(sess);
        logger.info("Endpoint connected: %s".formatted(sess));
    }

    @Override
    public void onWebSocketText(String message)
    {
        super.onWebSocketText(message);
        logger.info("Received TEXT message: %s".formatted(message));

        try {
            getRemote().sendString("Received" + message);
        } catch (IOException ignored) {
        }

        if (message.toLowerCase(Locale.US).contains("bye"))
        {
            getSession().close(StatusCode.NORMAL, "Thanks");
        }
    }

    @Override
    public void onWebSocketClose(int statusCode, String reason)
    {
        super.onWebSocketClose(statusCode, reason);
        logger.info("Socket Closed: [%s] %s".formatted(statusCode, reason));
        closureLatch.countDown();
    }

    @Override
    public void onWebSocketError(Throwable cause)
    {
        super.onWebSocketError(cause);
        cause.printStackTrace(System.err);
    }

    public void awaitClosure() throws InterruptedException
    {
        logger.info("Awaiting closure from remote");
        closureLatch.await();
    }
}

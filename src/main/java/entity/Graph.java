package entity;

import Operator.Operator;
import com.vesoft.nebula.client.graph.NebulaPoolConfig;
import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.AuthFailedException;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.exception.NotValidConnectionException;
import com.vesoft.nebula.client.graph.net.NebulaPool;
import com.vesoft.nebula.client.graph.net.Session;
import error.ExecuteException;

import java.io.UnsupportedEncodingException;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;

public  class Graph {
    public   String username = "root";
    public   String password = "nebula";
    public   String spaceName = null;
    public   String host = "127.0.0.1";
    public   int port = 9669;
    public   NebulaPoolConfig nebulaPoolConfig = new NebulaPoolConfig();

    public Graph() {

    }

    public Graph(String host, int port, String username, String password, String spaceName){
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;
        this.spaceName = spaceName;
    }


    public Graph(String host, int port, String username, String password, String spaceName,NebulaPoolConfig nebulaPoolConfig){
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;
        this.spaceName = spaceName;
        this.nebulaPoolConfig = nebulaPoolConfig;
    }


    public Session getSession(boolean reconnect){
        NebulaPool nebulaPool = new NebulaPool();
        Session session = null;
        List<HostAddress> addresses = Arrays.asList(new HostAddress(host,port));
        ResultSet execute = null;
        try {
            nebulaPool.init(addresses, nebulaPoolConfig);
            session = nebulaPool.getSession(username, password, reconnect);
            String useSpace = "USE " + spaceName + ";";
            execute = session.execute(useSpace);
        } catch (UnknownHostException | NotValidConnectionException | IOErrorException | AuthFailedException | UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        assert execute != null;
        if (!execute.isSucceeded()) {
            throw new ExecuteException("space is not found");
        }

        return session;
    }

    public ResultSet run(String nGql){
        Session session = getSession(false);
        ResultSet execute = null;
        try {
            execute = session.execute(nGql);
        } catch (IOErrorException | UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        assert execute != null;
        if (!execute.isSucceeded()) {
            throw new ExecuteException (execute.getErrorMessage());
        }
        return execute;
    }

    public ResultSet show_Tags(){
        return run(Operator.SHOW_TAGs);
    }

}

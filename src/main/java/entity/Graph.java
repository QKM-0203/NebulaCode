/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import error.ExecuteException;

import java.io.UnsupportedEncodingException;

public class Graph extends GraphService{

    private final Session session;
    private String  spaceName;

    protected Graph(String spaceName, Session session) {
        this.session = session;
        //use session execute "use spaceName"
        ResultSet execute = null;

        String useSpace = "USE " + spaceName;
        try {
            execute = session.execute(useSpace);
        } catch (IOErrorException | UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        assert execute != null;
        if (!execute.isSucceeded()) {
            throw new ExecuteException("space is not found");
        }

    }

    public ResultSet run(String nGql) {
        ResultSet execute = null;
        try {
            execute = session.execute(nGql);
        } catch (IOErrorException | UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        assert execute != null;
        if (!execute.isSucceeded()) {
            throw new ExecuteException(execute.getErrorMessage());
        }
        return execute;
    }



}

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
import java.util.HashMap;
import java.util.List;

public class Graph {

    private final Session session;

    protected Graph(String spaceName, Session session) {
        this.session = session;
        //use session execute "use spaceName"
        ResultSet  result = null;

        String useSpace = "USE " + spaceName;
        try {
            result = session.execute(useSpace);
        } catch (IOErrorException | UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        if( result == null){
            throw new ExecuteException("session is broken");
        }
        if (! result.isSucceeded()) {
            throw new ExecuteException("space is not found");
        }

    }

    public ResultSet run(String nGql) throws IOErrorException, UnsupportedEncodingException {
       return session.execute(nGql);
    }

    /**
     *
     * @param graphObject graphObject can be Node or Relationship or Subgraph or path
     */
    public void create(Object graphObject){

    }

    /**
     *
     * @param graphObject delete vertex first delete relationship about vertex
     */
    public void delete(Object graphObject){


    }

    public void delete_all(){

    }

    public void update(Object graphObject){


    }

    public boolean exists(Object graphObject){
        return true;
   }

    /**
     * Integer is index length
     * @param Name schemaName
     * @param indexName indexName
     * @param propList indexList
     * @return  if success
     */
    public boolean createSchemaIndex(String Name, String indexName,HashMap<String,Integer> propList){
       return true;
   }

    public List<String> getTags(){
        return null;
   }

    public List<String> getEdges(){
        return null;
   }

    public void createIndex(Schema tag,String indexName,HashMap<String,Integer> propList){
   }

    public void dropIndex(Schema tag){
   }

    public List<String> getTagIndexes(String name){
        return null;
   }


    private ResultSet query(String nGql) throws IOErrorException, UnsupportedEncodingException {
        return run(nGql);
   }

    public boolean createSchema(Schema schema){
        return true;
    }

    public boolean dropSchema(String name){
        return true;
    }

    /**
     *  add property or update property DateType or update TTL,if is null don not modify
     */
    public boolean modifySchema(String name,Schema schema){
        return true;
    }



}

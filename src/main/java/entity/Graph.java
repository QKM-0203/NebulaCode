/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import Operator.DateType;
import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.client.graph.exception.IOErrorException;
import com.vesoft.nebula.client.graph.net.Session;
import error.ExecuteException;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.List;

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


    //model can be Node or Relationship  or  Subgraph
    public void create(Object model){

    }



    //delete vertex first delete relationship about vertex
    public void delete(Object model){


    }


    public void delete_all(){

    }

    public void update(Object model){


    }


   public boolean exists(Object model){
        return true;
   }


   //Integer is index length
   public boolean createTagIndex(String tagName, HashMap<String,Integer> propList){
       return true;
   }

   public boolean createEdgeName(String edge,HashMap<String,Integer> propList){
        return true;
   }

   public List<String> getTags(){
        return null;
   }

   public List<String> getEdges(){
        return null;
   }

   public boolean createIndex(String name,List<String> propList){
        return true;
   }

   public boolean dropTagIndex(String name,List<String> propList){
        return true;
   }

   public ResultSet getTagIndexes(String name){
        return null;
   }

   public ResultSet query(String nGql,HashMap<String,Object> propList){
        return null;
   }

    public boolean createSchema(Schema schema){
        return true;
    }

    public boolean dropSchema(String name){
        return true;
    }

    //add property or update property
    public boolean modifySchema(String name,HashMap<String, DateType> propMap){
        return true;
    }






}

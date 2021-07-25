/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;



import java.util.HashMap;
import java.util.List;


public class Vertex extends Entity{

    //vertexVid
    private Object vid;


    private HashMap<String, HashMap<String,Object>> propMap;


    public Vertex(Object vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = vid;
        this.propMap = propMap;
    }

    public void setVid(Object vid) {
        this.vid = vid;
    }

    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
        this.propMap = propMap;
    }

    public Graph graph(){
        return graph;
    }

    public List<String> getTags(){
        return null;
    }

    public boolean isHasTag(String tagName){
        return true;
    }

    public boolean addTag(String name,HashMap<String,Object> propMap){
        //update local
        //update db (insert vertex)
        return true;
    }


    //delete vertex
    public boolean clearAllTags(){
        graph.delete(this);
        return true;
    }

    //update propertyValue of tag
    public boolean updateTag(String name,HashMap<String,Object> propMap){
        //judge vertex if exist in graph
        return true;
    }


}

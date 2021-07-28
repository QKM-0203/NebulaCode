/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;




import java.util.*;


public class Vertex extends Entity{

    //vertexVid
    private Object vid;


    private HashMap<String, HashMap<String,Object>> propMap;


    public Vertex(Object vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = vid;
        this.propMap = propMap;
    }

    public Object getVid() {
        return vid;
    }

    public void setVid(Object vid) {
        this.vid = vid;
    }

    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
        this.propMap = propMap;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Vertex vertex = (Vertex) o;
        return Objects.equals(vid, vertex.vid);
    }

    @Override
    public int hashCode() {
        return Objects.hash(vid, propMap);
    }

    public Graph graph(){
        return graph;
    }

    public List<String> getTags(){
        return null;
    }

    public boolean hasTag(String tagName){
        return true;
    }

    public boolean addTag(String name,HashMap<String,Object> propMap){
        //update local
        //update db (insert vertex)
        return true;
    }


    /**
     * delete vertex
     */
    public boolean clearAllTags(){
        graph.delete(this);
        return true;
    }

    /**
     * update propertyValue of tag
     */
    public boolean updateTag(String name,HashMap<String,Object> propMap){
        //judge vertex if exist in graph
        return true;
    }


    //(1 :QKM3{name: "asd", age: 19} :QKM2{name: "asd", age: 19})
    @Override
    public String toString() {
       String result = (vid instanceof String ) ? "("+"\""+"%s"+"\""+"%s" +")" :
               "("+"%s"+"%s" +")";
        StringBuilder part = new StringBuilder();
        for (String tagName : propMap.keySet()) {
            HashMap<String, Object> propValueMap = propMap.get(tagName);
            part.append(String.format(" :%s{",tagName));
            ArrayList<String> prop = new ArrayList<>();
            for (String propName : propValueMap.keySet()) {
                if(propValueMap.get(propName) instanceof String){
                    prop.add(String.format("%s: "+"\""+"%s"+"\"",propName,propValueMap.get(propName)));
                }else{
                    prop.add(String.format("%s: "+"%s",propName,propValueMap.get(propName)));
                }
            }
            part.append(String.join(", ", prop)).append("}");
        }
       return String.format(result,vid,part);
    }


}

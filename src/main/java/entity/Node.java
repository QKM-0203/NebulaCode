package entity;

import java.util.*;


public class Node {

    //vertexList
    private List<Object> vid;

     //propMap
    private HashMap<String, HashMap<String,Object>> propMap;

    public Node(List<Object> vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = vid;
        this.propMap = propMap;
    }

    public Node(Object vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = new ArrayList<>();
        this.vid.add(vid);
        this.propMap = propMap;
    }

    public Node() {
    }

    public void setVid(List<Object> vid) {
        this.vid = vid;
    }


    public void setVid(Object vid) {
        this.vid.add(vid);
    }


    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
        this.propMap = propMap;
    }
}

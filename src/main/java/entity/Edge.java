package entity;

import java.util.HashMap;


public class Edge {

    private Object srcVid;

    private Object dstVid;

    private int rank;

    //propMap
    private HashMap<String, HashMap<String,Object>> propMap;

    public Edge(Object srcVid, Object dstVid, HashMap<String, HashMap<String, Object>> propMap) {
        this.srcVid = srcVid;
        this.dstVid = dstVid;
        this.propMap = propMap;
    }

    public Edge(Object srcVid, Object dstVid, int rank,HashMap<String, HashMap<String, Object>> propMap) {
        this.srcVid = srcVid;
        this.dstVid = dstVid;
        this.rank = rank;
        this.propMap = propMap;
    }

    public void setSrcVid(Object srcVid) {
        this.srcVid = srcVid;
    }

    public void setDstVid(Object dstVid) {
        this.dstVid = dstVid;
    }

    public void setRank(int rank) {
        this.rank = rank;
    }

    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
        this.propMap = propMap;
    }
}

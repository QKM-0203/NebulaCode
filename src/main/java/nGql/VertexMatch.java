package nGql;


import Operator.Special;
import com.vesoft.nebula.client.graph.data.ResultSet;
import entity.Graph;
import entity.Vertex;

import java.util.HashMap;
import java.util.List;

public class VertexMatch {

    private Graph graph;
    private String tagName;
    private HashMap<String,Object> propMap;
    private long skip;
    private long limit;
    private String orderBy;
    private Special special = Special.ASC;
    private String groupBy;
    private HashMap<String,Condition> conMap;


    protected VertexMatch(Graph graph) {
         this.graph = graph;
    }

    public  VertexMatch init(String tagName, HashMap<String, Object> propMap) {
        this.tagName = tagName;
        this.propMap = propMap;
        return this;
    }

    public VertexMatch where(HashMap<String,Condition> conMap){
        this.conMap = conMap;
        return this;
    }

    public VertexMatch skip(long skip){
        this.skip = skip;
        return this;
    }

    public VertexMatch orderBy(String name,Special special){
        this.orderBy = name;
        this.special = special;
        return this;
    }

    public VertexMatch groupBy(String name){
        this.groupBy = name;
        return this;
    }

    public VertexMatch limit(long limit){
        this.limit = limit;
        return this;
    }

    public Vertex first(){
        return query().get(0);
    }

    private String connectParameters(){
        return "";
    }

    //查询的结果
    public List<Vertex> query(){
        String s = connectParameters();
        ResultSet run = graph.run(s);
        //将run转成对象
        return null;
    }

   public long count(){
        return query().size();
   }

   public boolean exist(){
        return count()>0;
   }


}

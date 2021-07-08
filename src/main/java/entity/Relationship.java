package entity;

import java.util.Map;

public class Relationship {

    //开始节点
    private Node startNode;

    //结束节点
    private Node endNode;

    //边类型的属性
    private Map<String, Object>  type;

    //边的rank
    private int rank;

    //边类型的名字
    private String relationshipName;

    public Relationship(Node startNode, Node endNode, Map<String, Object> type, int rank, String relationshipName) {
        this.startNode = startNode;
        this.endNode = endNode;
        this.type = type;
        this.rank = rank;
        this.relationshipName = relationshipName;
    }

    //插入一条边
    public boolean  insert(){
        return true;
    }

    //返回具有该边关系的所有的集合
    public String type(){
        return "";
    }
    //...

}

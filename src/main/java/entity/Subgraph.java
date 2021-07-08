package entity;


import java.util.List;

public class Subgraph {

    //子图中的边类型名称
    private List<String> edgeNameList;

    //子图中的节点的vids
    private List<String> nodeVids;

    //某一个vid
    private String nodeVid;

    //某一个边类型
    private String edgeName;

    //子图中的起始节点
    private List<Node> nodeList;

    //步长
    private int step;
}

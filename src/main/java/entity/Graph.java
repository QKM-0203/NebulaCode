package entity;

//执行NGQl语句的操作
public class Graph {

    //连接图数据库的uri
    private String uri;

    //图空间的名称
    private String spaceName;

    static{
        //连接数据库的操作
    }

    public Graph(String uri, String spaceName) {
        this.uri = uri;
        this.spaceName = spaceName;
    }


    //更新
    public boolean update(String nGQl){
        return true;
    }

    //...增加，删除，查找

}

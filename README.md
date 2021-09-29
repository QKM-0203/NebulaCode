# NebulaCode 项目

***
NebulaCode 是一个基于 [nebula-java](https://github.com/vesoft-inc/nebula-java)
来让用户更方便操作 [Nebula Group](https://nebula-graph.com.cn/ ) 的工具。

### 特性

* 封装了图相关的对象，比如 GraphService, Graph, Subgraph, Vertex, Edge, Path 等。
* 提供了图的增删改查以及schema相关操作。
* 提供了 nGQL 和 Match 包装的方法，用户根据不同方法执行相应的 query。

### 使用
* 创建并连接 space

```java
public class Connect {
    public static void main(String[] args) {
        GraphService graphService = new GraphService(
            Arrays.asList(new HostAddress("127.0.0.1", 9669),
                new HostAddress("127.0.0.1", 9898)),
            "root", "nebula", false);
        Space test = new Space("test", 10, 1, DataType.INT64);
        graphService.createSpace(test);
        Graph test = graphService.getGraph("test");
    }
}
```

* 创建 schema

```java
public class createSchema {
    public static void main(String[] args) {
        List<Property> tagProperties = new ArrayList<>();
        Property name = new Property("name", DataType.STRING.setLength(10), true, null);
        tagProperties.add(name);
        Schema person = new Schema("person", tagProperties, 0, null);
        test.createTag(person);
    }
}

```

* 创建 vertex

```java
 import com.vesoft.nebula.ngqlbuilder.entity.Vertex;

public class createVertex {
    public static void main(String[] args) {
        //first must has tag(person --- (string name) )
        HashMap<String, HashMap<String, Object>> tags = new HashMap<>();
        HashMap<String, Object> personValue = new HashMap<>();
        personValue.put("name", "alice");
        tags.put("person", personValue);
        Vertex vertex = new Vertex(1, tags);
        test.create(vertex);
    }
}
```
***
更多增加、删除、查询,参照/src/main/test

* push

****
当本地数据发生更改时，可以使用push方法推送到数据库服务（参照/src/main/test/TestPush）。

* pull

*****
使用pull方法获取数据库服务最新数据（参照/src/main/test/TestPull）。

import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.entity.Vertex;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import org.junit.Test;

public class TestPull extends TestDataBase {
    @Test
    public void testCanNotPullOtherObject() {
        StringBuffer qkm = new StringBuffer("qkm");
        try {
            graph.pull(qkm);
        } catch (Exception e) {
            assert e.getMessage().equals("java.lang.StringBuffer object is not support");
        }
    }

    @Test
    public void testCanPullVertex() throws UnsupportedEncodingException {
        HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
        propMap.put("QKM5", null);
        Vertex vertex = new Vertex(null, propMap);
        vertex.setVid(vertexOne.getVid());
        graph.create(vertexOne);
        graph.pull(vertex);
        assert vertex.getTagNames().equals(vertexOne.getTagNames());
        assert !vertex.getTagNames().contains("QKM5");
    }

    @Test
    public void testPullRelationship() throws UnsupportedEncodingException {
        HashMap<String, Object> edgeValue = new HashMap<>();
        edgeValue.put("teacher", "QI");
        Relationship relationship = new Relationship("1", "2", "team", edgeValue, 1);
        graph.pull(relationship);
        assert relationship.getPropMap().get("teacher").toString().equals("Sun");
    }

    @Test
    public void testPullSubgraph() throws UnsupportedEncodingException {
        graph.create(subgraph);
        graph.run("upsert edge on team \"1\"->\"2\"@1 set teamName = \"XuXin\"");
        graph.run("upsert vertex on QKM4 \"3\" set sex = \"女\"");
        graph.pull(subgraph);
        assert subgraph.getVertexes().get(2).getPropMap().get("QKM4")
            .get("sex").toString().equals("女");
        assert subgraph.getRelationships().get(0).getPropMap()
            .get("teamName").toString().equals("XuXin");
    }

}

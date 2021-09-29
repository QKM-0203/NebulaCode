/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.ngqlbuilder.entity.Schema;
import java.util.HashMap;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestRun extends TestDataBase {
    {
        graph.createTag(person);
        graph.createTag(hobby);
        graph.createEdge(subject);
        graph.createEdge(work);
        graph.create(vertexOne);
        graph.create(vertexTwo);
        graph.create(vertexThird);
        graph.create(vertexFour);
        graph.create(relationship12);
        graph.create(relationship32);
        graph.create(relationship24);
        graph.create(relationship34);
        graph.create(relationship23);
        graph.create(relationship13);
    }

    @Test
    public void testRunCreateSpace() {
        ResultSet run = graph.run("CREATE SPACE my_space_4(vid_type = FIXED_STRING(30))");
        assert run.isSucceeded();
    }

    @Test
    public void testRunDropSpace() {
        ResultSet run = graph.run("DROP SPACE my_space_4");
        assert run.isSucceeded();
    }

    @Test
    public void testRunCreateTag() {
        ResultSet run = graph.run("CREATE TAG player(name string, age int)");
        assert run.isSucceeded();
    }

    @Test
    public void testRunDropTag() {
        ResultSet run = graph.run("DROP TAG IF EXISTS player");
        assert run.isSucceeded();
    }

    @Test
    public void testRunCreateEdge() {
        ResultSet run = graph.run("CREATE EDGE IF NOT EXISTS subject(name string)");
        assert run.isSucceeded();
    }

    @Test
    public void testRunDropEdge() {
        ResultSet run = graph.run("DROP EDGE IF EXISTS player");
        assert run.isSucceeded();
    }

    @Test
    public void testRunInsertVertex() {
        Schema tag = new Schema("QKM5", null, 0, null);
        graph.createTag(tag);
        ResultSet run = graph.run("INSERT VERTEX QKM5 VALUES 7:()");
        assert run.isSucceeded();
    }

    @Test
    public void testRunInsertRelationship() {
        Schema edge = new Schema("QKM8", null, 0, null);
        graph.createEdge(edge);
        ResultSet run = graph.run("INSERT EDGE QKM8 VALUES 7->8@1:()");
        assert run.isSucceeded();
    }

    @Test
    public void testRunMatchVertex() {
        HashMap<String, Integer> indexOnperson = new HashMap<>();
        indexOnperson.put("name", 10);
        graph.createTagIndex("person", "i_person_name", indexOnperson);
        graph.run("REBUILD TAG INDEX i_person_mame");
        ResultSet run = graph.run("MATCH (v:person{name: \"qkm\"}) RETURN v");
        assert run.isSucceeded();
    }

    @Test
    public void testRunMatchRelationship() {
        graph.createTagIndex("person", "i_person", null);
        graph.run("REBUILD TAG INDEX i_person");
        ResultSet run = graph.run("MATCH (v:person)-[e:subject]->(v1) RETURN e");
        assert run.isSucceeded();
    }

    @Test
    public void testRunLookUp() {
        graph.createEdgeIndex("subject", "i_subject", null);
        graph.run("REBUILD TAG INDEX i_subject");
        ResultSet run = graph.run("LOOKUP ON subject YIELD subject.object");
        assert run.isSucceeded();
    }

    @Test
    public void testRunFetchVertex() {
        ResultSet run = graph.run("FETCH PROP ON person 1 YIELD person.name");
        assert run.isSucceeded();
    }

    @Test
    public void testRunFetchRelationship() {
        ResultSet run = graph.run("FETCH PROP ON subject 1->2,3->4 YIELD subject.object");
        assert run.isSucceeded();
    }

    @Test
    public void testRunFindPath() {
        ResultSet run = graph.run("FIND SHORTEST PATH FROM 1 TO 2 OVER *");
        assert run.isSucceeded();
    }

    @Test
    public void testRunGetSubgraph() {
        ResultSet run = graph.run("GET SUBGRAPH 1 STEPS FROM 1,2");
        assert run.isSucceeded();
    }

    @Test
    public void testRunGo() {
        ResultSet run = graph.run("GO 0 TO 1 STEPS FROM 1,2,3 OVER * YIELD subject.object as object "
            + "| ORDER BY object | GROUP BY $-.object YIELD $-.object AS object,COUNT(*) AS count "
            + "| ORDER BY object");
        assert run.isSucceeded();
    }

}

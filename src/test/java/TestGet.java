/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.client.graph.data.HostAddress;
import com.vesoft.nebula.client.graph.data.ValueWrapper;
import com.vesoft.nebula.orm.entity.Graph;
import com.vesoft.nebula.orm.entity.GraphService;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;

public class TestGet {
    private GraphService graphService = new GraphService(
        Arrays.asList(new HostAddress("127.0.0.1",9669),
            new HostAddress("127.0.0.1",9898)),
        "root","nebula",false);
    private Graph graph = graphService.getGraph("test");

    @Test
    public void testGetTags() {
        List<ValueWrapper> tags = graph.getTags();
        assert  tags.toString().equals("[\"QKM1\", \"QKM2\", \"QKM3\", \"QKM4\", "
            + "\"QKM5\"]");
    }

    @Test
    public void testGetEdges() {
        List<ValueWrapper> tags = graph.getEdges();
        assert  tags.toString().equals("[\"QKM6\", \"team\", \"work\"]");
    }
}

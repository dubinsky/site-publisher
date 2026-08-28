import { rewritePath } from "./rewrite.js";
import aliases from "./collection-aliases.json";

export default {
  async fetch(request) {
    const url = new URL(request.url);
    const rewritten = rewritePath(url.pathname, aliases);
    if (rewritten == null || rewritten === url.pathname) {
      return fetch(request);
    }
    url.pathname = rewritten;
    return fetch(new Request(url, request));
  }
};

<script lang="ts">
  import type { PageProps } from './$types';
  let { data } : PageProps = $props()
</script>

<div>
  <p>hello hello</p>
  <ul>
    {#each data.trips as trip}
      <li>
	{JSON.stringify(trip)}
      </li>
    {/each}
  </ul>
</div>

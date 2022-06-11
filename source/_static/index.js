
document.getElementById('templates').innerHTML = `<table jput="t_template">
<tbody jput="quickstart">
 <td><i class="fas fa-book"></i> <a href="https://henilp105.github.io/fortran-lang.org{{link}}"> {{title}}</a><br>
   {{description}}<br></td>
  
</tbody>
<tbody jput="other_reference">
 <ul>
  <tr>
    <td><li>
    <a href="{{url}}" target="_blank"><b>
      {{name}}</b></a>
   {{description}}</td></li>
  </tr></ul>
</tbody>


<tbody jput="online_courses">
  <ul>
    <tr>
      <td><li>
    <a href="{{url}" target="_blank"><b>
      {{name}}</b></a>
   {{description}}</td></li>
  </tr>
</tbody>

<tbody jput="books">
  <ul>
    <tr>
      <td><li>
    <td><i>{{author}}</i>{{year}}. <a href="{{url}}" target="_blank"><b>
      {{title}}
    </b></a>.
    {{edition}}.
    {{location}}.
    {{publisher}}
  </td></li>
  </tr>
</tbody>
</table>`;

